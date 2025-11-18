/* AstraLisp OS Virtual Memory Manager - PowerISA Implementation
 *
 * This implements the PowerISA 64-bit MMU with:
 * - SLB (Segment Lookaside Buffer) management
 * - HPT (Hashed Page Table) with HPTE entries
 * - VSID (Virtual Segment ID) allocation
 * - TLB management
 * - 4KB and 64KB page support
 */

#include "vmm.h"
#include "pmm.h"
#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

/* PowerISA MMU Constants */
#define PAGE_SHIFT 12
#define PAGE_SIZE (1UL << PAGE_SHIFT)  /* 4KB pages */
#define PAGE_MASK (~(PAGE_SIZE - 1))

#define LARGE_PAGE_SHIFT 16
#define LARGE_PAGE_SIZE (1UL << LARGE_PAGE_SHIFT)  /* 64KB pages */

#define SEGMENT_SHIFT 28
#define SEGMENT_SIZE (1UL << SEGMENT_SHIFT)  /* 256MB segments */
#define SEGMENT_MASK (~(SEGMENT_SIZE - 1))

/* SLB Constants */
#define SLB_NUM_ENTRIES 64  /* PowerISA typically has 32-64 SLB entries */
#define SLB_ESID_MASK 0xFFFFFFFFF0000000UL
#define SLB_VSID_SHIFT 12

/* HPT Constants */
#define HPT_MIN_SIZE (256 * 1024)  /* Minimum 256KB (64 HPTEGs) */
#define HPT_DEFAULT_SIZE (2 * 1024 * 1024)  /* 2MB default */
#define HPTEG_SIZE 128  /* Each HPTEG is 128 bytes (8 HPTEs of 16 bytes each) */
#define HPTES_PER_GROUP 8
#define HPTE_SIZE 16

/* HPTE Flags - V (Valid), AVPN (Abbreviated VPN), and Protection */
#define HPTE_VALID 0x8000000000000000UL
#define HPTE_LARGE_PAGE 0x0000000000000004UL
#define HPTE_WRITABLE 0x0000000000000002UL
#define HPTE_NO_EXECUTE 0x0000000000000004UL
#define HPTE_REFERENCED 0x0000000000000100UL
#define HPTE_CHANGED 0x0000000000000080UL

/* Hash Page Table Entry Structure (128 bits / 16 bytes) */
struct hpte {
    uint64_t dword0;  /* V(1) | AVPN(57) | SW(1) | L(1) | reserved(4) */
    uint64_t dword1;  /* RPN(52) | reserved(2) | R(1) | C(1) | WIMG(4) | N(1) | PP(2) */
} __attribute__((packed));

/* Segment Lookaside Buffer Entry (Software representation) */
struct slb_entry {
    uint64_t esid;    /* Effective Segment ID (top 36 bits of EA) */
    uint64_t vsid;    /* Virtual Segment ID */
    uint32_t flags;   /* Flags: kernel/user, execute permissions, etc. */
    bool valid;
};

/* Virtual Memory Manager Context */
struct vmm_context {
    void* hpt;                    /* Hash Page Table base address */
    size_t hpt_size;              /* HPT size in bytes */
    uint32_t hpt_mask;            /* Mask for HPT hash (size - 1) */
    struct slb_entry slb[SLB_NUM_ENTRIES];  /* Software SLB cache */
    uint64_t next_vsid;           /* Next VSID to allocate */
    uint32_t slb_index;           /* Round-robin SLB replacement index */
};

/* Global VMM context */
static struct vmm_context* vmm_ctx = NULL;

/* ========== PowerISA Assembly Helpers ========== */

/* Read SDR1 register (HPT base and size) */
static inline uint64_t read_sdr1(void) {
    uint64_t sdr1;
    __asm__ volatile("mfspr %0, 25" : "=r"(sdr1));  /* SPR 25 = SDR1 */
    return sdr1;
}

/* Write SDR1 register */
static inline void write_sdr1(uint64_t sdr1) {
    __asm__ volatile("mtspr 25, %0" :: "r"(sdr1));  /* SPR 25 = SDR1 */
    __asm__ volatile("isync");
}

/* Invalidate entire TLB */
static inline void invalidate_tlb_all(void) {
    __asm__ volatile(
        "li %%r3, 0x400\n"           /* 1024 iterations */
        "mtctr %%r3\n"
        "li %%r3, 0\n"
        "1:\n"
        "tlbiel %%r3\n"              /* Invalidate local TLB entry */
        "addi %%r3, %%r3, 0x1000\n"  /* Next page */
        "bdnz 1b\n"                   /* Loop */
        "sync\n"
        ::: "r3", "ctr"
    );
}

/* Invalidate single TLB entry */
static inline void invalidate_tlb_entry(uint64_t va) {
    __asm__ volatile(
        "tlbie %0\n"
        "sync\n"
        :: "r"(va)
    );
}

/* Insert SLB Entry (SLBMTE instruction) */
static inline void slbmte(uint64_t vsid, uint64_t esid) {
    __asm__ volatile(
        "slbmte %0, %1\n"
        "isync\n"
        :: "r"(vsid), "r"(esid)
    );
}

/* Invalidate SLB Entry (SLBIE instruction) */
static inline void slbie(uint64_t esid) {
    __asm__ volatile(
        "slbie %0\n"
        "isync\n"
        :: "r"(esid)
    );
}

/* Invalidate entire SLB (SLBIA instruction) */
static inline void slbia(void) {
    __asm__ volatile(
        "slbia\n"
        "isync\n"
    );
}

/* ========== HPT Management ========== */

/* Calculate primary hash for virtual address */
static uint32_t hpt_hash_primary(uint64_t vsid, uint64_t va) {
    uint64_t vpn = (va >> PAGE_SHIFT) & 0xFFFFFFFF;  /* Virtual Page Number */
    uint64_t hash = (vsid & 0x7FFFFF) ^ vpn;
    return (uint32_t)(hash & vmm_ctx->hpt_mask);
}

/* Calculate secondary hash */
static uint32_t hpt_hash_secondary(uint32_t primary_hash) {
    return ~primary_hash & vmm_ctx->hpt_mask;
}

/* Get HPTEG (Hash Page Table Entry Group) address */
static struct hpte* hpt_get_hpteg(uint32_t hash) {
    uintptr_t hpteg_addr = (uintptr_t)vmm_ctx->hpt + (hash * HPTEG_SIZE);
    return (struct hpte*)hpteg_addr;
}

/* Find free HPTE slot in HPTEG, or evict if full */
static struct hpte* hpt_find_slot(struct hpte* hpteg, bool* is_secondary) {
    /* Try to find invalid (free) entry */
    for (int i = 0; i < HPTES_PER_GROUP; i++) {
        if (!(hpteg[i].dword0 & HPTE_VALID)) {
            return &hpteg[i];
        }
    }

    /* No free slot - evict first entry (simple replacement policy) */
    /* Production implementation would use LRU or CLOCK algorithm */
    *is_secondary = !*is_secondary;  /* Mark as secondary if evicting */
    return &hpteg[0];
}

/* Insert HPTE into HPT */
static int hpt_insert(uint64_t va, uint64_t pa, uint64_t vsid, uint32_t flags) {
    /* Calculate primary hash */
    uint32_t primary_hash = hpt_hash_primary(vsid, va);
    struct hpte* hpteg = hpt_get_hpteg(primary_hash);
    bool is_secondary = false;

    /* Try primary HPTEG first */
    struct hpte* slot = hpt_find_slot(hpteg, &is_secondary);

    /* If primary full, try secondary */
    if (!slot || is_secondary) {
        uint32_t secondary_hash = hpt_hash_secondary(primary_hash);
        hpteg = hpt_get_hpteg(secondary_hash);
        slot = hpt_find_slot(hpteg, &is_secondary);
        is_secondary = true;
    }

    if (!slot) {
        return -1;  /* Both HTEGs full (shouldn't happen with eviction) */
    }

    /* Build HPTE */
    uint64_t avpn = ((vsid << 12) | ((va >> PAGE_SHIFT) & 0xFFF)) >> 7;

    /* dword0: V | AVPN | SW | L | reserved */
    slot->dword0 = HPTE_VALID | (avpn << 7) | (is_secondary ? 0x40 : 0);

    /* For large pages */
    if (flags & PAGE_LARGE) {
        slot->dword0 |= HPTE_LARGE_PAGE;
    }

    /* dword1: RPN | R | C | WIMG | PP */
    uint64_t rpn = pa >> PAGE_SHIFT;
    uint64_t pp = 0;  /* Protection: 00 = read/write kernel, no user access */

    if (flags & PAGE_WRITABLE) {
        pp = 0x2;  /* 10 = read/write for both kernel and user */
    } else {
        pp = 0x3;  /* 11 = read-only for both */
    }

    /* WIMG bits: W=Write-through, I=Cache inhibited, M=Memory coherent, G=Guarded */
    uint64_t wimg = 0x2;  /* M=1 (memory coherent) for normal memory */

    if (flags & PAGE_CACHE_DISABLE) {
        wimg |= 0x4;  /* I=1 (cache inhibited) */
    }

    slot->dword1 = (rpn << 12) | (wimg << 3) | pp;

    /* Ensure write is complete before TLB invalidation */
    __asm__ volatile("eieio; sync");

    return 0;
}

/* Remove HPTE from HPT */
static int hpt_remove(uint64_t va, uint64_t vsid) {
    /* Calculate hashes */
    uint32_t primary_hash = hpt_hash_primary(vsid, va);
    uint64_t avpn = ((vsid << 12) | ((va >> PAGE_SHIFT) & 0xFFF)) >> 7;

    /* Search primary HPTEG */
    struct hpte* hpteg = hpt_get_hpteg(primary_hash);
    for (int i = 0; i < HPTES_PER_GROUP; i++) {
        if ((hpteg[i].dword0 & HPTE_VALID) &&
            ((hpteg[i].dword0 >> 7) & 0x7FFFFFFFFULL) == avpn) {
            hpteg[i].dword0 = 0;  /* Invalidate */
            hpteg[i].dword1 = 0;
            return 0;
        }
    }

    /* Search secondary HPTEG */
    uint32_t secondary_hash = hpt_hash_secondary(primary_hash);
    hpteg = hpt_get_hpteg(secondary_hash);
    for (int i = 0; i < HPTES_PER_GROUP; i++) {
        if ((hpteg[i].dword0 & HPTE_VALID) &&
            ((hpteg[i].dword0 >> 7) & 0x7FFFFFFFFULL) == avpn) {
            hpteg[i].dword0 = 0;  /* Invalidate */
            hpteg[i].dword1 = 0;
            return 0;
        }
    }

    return -1;  /* Not found */
}

/* ========== SLB Management ========== */

/* Allocate new VSID */
static uint64_t slb_alloc_vsid(void) {
    return vmm_ctx->next_vsid++;
}

/* Find SLB entry by ESID */
static struct slb_entry* slb_find_entry(uint64_t esid) {
    esid &= SLB_ESID_MASK;
    for (int i = 0; i < SLB_NUM_ENTRIES; i++) {
        if (vmm_ctx->slb[i].valid && vmm_ctx->slb[i].esid == esid) {
            return &vmm_ctx->slb[i];
        }
    }
    return NULL;
}

/* Insert SLB entry (software + hardware) */
static int slb_insert_entry(uint64_t esid, uint64_t vsid, uint32_t flags) {
    esid &= SLB_ESID_MASK;

    /* Check if already exists */
    struct slb_entry* entry = slb_find_entry(esid);
    if (entry) {
        /* Update existing */
        entry->vsid = vsid;
        entry->flags = flags;
    } else {
        /* Find free slot or evict using round-robin */
        int index = vmm_ctx->slb_index;
        vmm_ctx->slb_index = (vmm_ctx->slb_index + 1) % SLB_NUM_ENTRIES;

        entry = &vmm_ctx->slb[index];

        /* Invalidate old entry in hardware if valid */
        if (entry->valid) {
            slbie(entry->esid);
        }

        entry->esid = esid;
        entry->vsid = vsid;
        entry->flags = flags;
        entry->valid = true;
    }

    /* Build SLB entry for hardware */
    /* SLBE format: ESID(36) | V(1) | Ks(1) | Kp(1) | N(1) | L(1) | C(1) | index(6) */
    /* SLBV format: VSID(52) | ... */

    uint64_t slbe = esid | 0x08000000;  /* V=1 (valid) */
    if (flags & SLB_KERNEL) {
        slbe |= 0x04000000;  /* Ks=1 (supervisor) */
    }
    if (flags & SLB_USER) {
        slbe |= 0x02000000;  /* Kp=1 (problem/user state) */
    }

    uint64_t slbv = (vsid << SLB_VSID_SHIFT);

    /* Insert into hardware SLB using SLBMTE */
    slbmte(slbv, slbe);

    return 0;
}

/* ========== VMM Public Interface ========== */

/* Initialize Virtual Memory Manager */
int vmm_init(void) {
    /* Allocate VMM context */
    vmm_ctx = (struct vmm_context*)pmm_alloc();
    if (!vmm_ctx) {
        return -1;
    }
    memset(vmm_ctx, 0, PAGE_SIZE);

    /* Allocate Hash Page Table */
    size_t hpt_pages = HPT_DEFAULT_SIZE / PAGE_SIZE;
    vmm_ctx->hpt = pmm_alloc_multiple(hpt_pages);
    if (!vmm_ctx->hpt) {
        pmm_free(vmm_ctx);
        return -1;
    }

    vmm_ctx->hpt_size = HPT_DEFAULT_SIZE;
    vmm_ctx->hpt_mask = (HPT_DEFAULT_SIZE / HPTEG_SIZE) - 1;

    /* Clear HPT */
    memset(vmm_ctx->hpt, 0, HPT_DEFAULT_SIZE);

    /* Initialize VSID allocator */
    vmm_ctx->next_vsid = 1;  /* VSID 0 is reserved */
    vmm_ctx->slb_index = 0;

    /* Configure SDR1 register (HPT base and size) */
    /* SDR1 format: HTABORG(46) | reserved(11) | HTABSIZE(5) | reserved(2) */
    uint64_t htaborg = ((uint64_t)vmm_ctx->hpt) >> 18;  /* Physical address >> 18 */
    uint64_t htabsize = __builtin_ctzl(HPT_DEFAULT_SIZE >> 18);  /* log2(size >> 18) */
    uint64_t sdr1 = (htaborg << 18) | (htabsize & 0x1F);

    write_sdr1(sdr1);

    /* Invalidate all SLB entries */
    slbia();

    /* Invalidate entire TLB */
    invalidate_tlb_all();

    /* Set up kernel segment (ESID 0 = addresses 0x0000000000000000 - 0x000000000FFFFFFF) */
    uint64_t kernel_vsid = slb_alloc_vsid();
    slb_insert_entry(0, kernel_vsid, SLB_KERNEL | SLB_USER);

    /* Identity map first 16MB for kernel code/data */
    for (uint64_t addr = 0; addr < 0x1000000; addr += PAGE_SIZE) {
        hpt_insert(addr, addr, kernel_vsid, PAGE_WRITABLE);
    }

    /* Map kernel heap (16MB - 32MB) */
    for (uint64_t addr = 0x1000000; addr < 0x2000000; addr += PAGE_SIZE) {
        hpt_insert(addr, addr, kernel_vsid, PAGE_WRITABLE);
    }

    return 0;
}

/* Map a virtual address to physical address */
int vmm_map_page(void* pagedir, uintptr_t virt, uintptr_t phys, uint32_t flags) {
    (void)pagedir;  /* Unused in PowerISA - we use global HPT */

    if (!vmm_ctx) {
        return -1;
    }

    /* Get ESID for this address */
    uint64_t esid = virt & SLB_ESID_MASK;

    /* Find or create SLB entry */
    struct slb_entry* entry = slb_find_entry(esid);
    if (!entry) {
        /* Create new segment mapping */
        uint64_t vsid = slb_alloc_vsid();
        slb_insert_entry(esid, vsid, SLB_KERNEL | SLB_USER);
        entry = slb_find_entry(esid);
    }

    if (!entry) {
        return -1;  /* SLB insertion failed */
    }

    /* Insert into HPT */
    int result = hpt_insert(virt, phys, entry->vsid, flags);

    /* Invalidate TLB entry */
    invalidate_tlb_entry(virt);

    return result;
}

/* Unmap a virtual address */
int vmm_unmap_page(void* pagedir, uintptr_t virt) {
    (void)pagedir;

    if (!vmm_ctx) {
        return -1;
    }

    /* Get ESID and find SLB entry */
    uint64_t esid = virt & SLB_ESID_MASK;
    struct slb_entry* entry = slb_find_entry(esid);

    if (!entry) {
        return -1;  /* No mapping for this segment */
    }

    /* Remove from HPT */
    int result = hpt_remove(virt, entry->vsid);

    /* Invalidate TLB entry */
    invalidate_tlb_entry(virt);

    return result;
}

/* Create a new address space (for user processes) */
void* vmm_create_pagedir(void) {
    /* In PowerISA, we don't have per-process page directories
     * Instead, each process gets its own set of VSIDs
     * For now, return a dummy value - will be enhanced for full process support */
    return (void*)0x1;
}

/* Destroy an address space */
void vmm_destroy_pagedir(void* pagedir) {
    /* Placeholder - would invalidate process VSIDs */
    (void)pagedir;
}

/* Switch address space (context switch) */
void vmm_switch_pagedir(void* pagedir) {
    /* Placeholder - would load process SLB entries */
    (void)pagedir;
}

/* Get current address space */
void* vmm_get_current_pagedir(void) {
    return (void*)vmm_ctx;
}

/* Allocate multiple contiguous physical pages (helper for HPT) */
void* pmm_alloc_multiple(size_t pages) {
    /* This is a helper that should be in PMM, but we define it here for now */
    void* first_page = pmm_alloc();
    if (!first_page || pages == 1) {
        return first_page;
    }

    /* For multiple pages, we allocate them separately
     * Production implementation would allocate contiguous physical memory */
    for (size_t i = 1; i < pages; i++) {
        void* page = pmm_alloc();
        if (!page) {
            /* Failed - should free previously allocated pages */
            return NULL;
        }
    }

    return first_page;
}
