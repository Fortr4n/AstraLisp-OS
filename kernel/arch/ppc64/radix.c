/* AstraLisp OS - PowerISA Radix MMU Implementation */

#include "radix.h"
#include "../../mm/pmm.h"
#include "../../lib/string.h"

/* We need ffi definitions */
extern void ffi_write_spr(uint16_t spr, uint64_t val);
extern uint64_t ffi_read_spr(uint16_t spr);
/* We need assembly for tlbie */
extern void asm_tlbie(void); /* We should implement this in ffi.S or vectors.S */

/* Global Tables */
static struct patb_entry* partition_table = NULL;
static struct prtb_entry* process_table = NULL;

#define PATB_SIZE 4096 /* 1 Page sufficient for minimal LPIDs */
#define PRTB_SIZE 4096 /* 1 Page sufficient for minimal PIDs */

void radix_init_hypervisor(void) {
    /* 1. Allocate Partition Table */
    /* Needs natural alignment. 4KB is fine for small tables. */
    partition_table = (struct patb_entry*)pmm_alloc();
    memset(partition_table, 0, PATB_SIZE);
    
    /* 2. Allocate Process Table */
    process_table = (struct prtb_entry*)pmm_alloc();
    memset(process_table, 0, PRTB_SIZE);
    
    /* 3. Setup Partition Table Entry 0 (LPID 0 - The Host) */
    /* Structure: [ PRTB_PHYS | RTS ] [ Control ] */
    /* RTS=0x0C approx for full size ? Let's use 24-bit RTS for now (encoded value) */
    /* Actually Linux uses 0x18 for 52-bit? */
    /* Let's rely on standard RTS encoding: 0x5 = 52 bit? */
    /* PRTB Base must be aligned. */
    
    uint64_t prtb_phys = (uint64_t)process_table; /* Assuming identity mapping in early boot */
    
    /* Format:
       Bits 0-51: Base Addr
       Bits 52-55: Reserved
       Bits 56-58: RTS (Size = 2^(11+RTS) bytes? verify spec)
       Bit 63: Valid (in PATB)
    */
    
    /* If we use RTS=11 (Size 4MB approx), encoded as 0xB? */
    /* For Phase 1, just 1 page (4KB) process table is enough for a few PIDs. */
    /* Min size is 4KB. RTS encoding for 4KB? */
    /* Spec: Size = 2^12 * (RTS+1) ?  */
    
    /* Simple config: */
    uint64_t patb_val = prtb_phys | PATB_HR | 0x0; /* HR=1 (Host Radix), RTS=0 (min) */
    
    partition_table[0].prtb_phys = patb_val;
    partition_table[0].prtb_control = 0; /* No restrictions */
    
    /* 4. Write PTCR (Partition Table Control Register) */
    /* Format: | PATB Phys | 0... | RTS | */
    uint64_t patb_phys = (uint64_t)partition_table;
    uint64_t ptcr_val = patb_phys | 0x0; /* RTS=0 (min) */
    
    ffi_write_spr(SPR_PTCR, ptcr_val);
}

void radix_set_root(uint64_t pid, uint64_t pml4_phys) {
    if (!process_table) return;
    
    /* Index by PID */
    /* Check bounds if PRTB_SIZE limited */
    
    struct prtb_entry* entry = &process_table[pid];
    
    /* Encode Root Pointer */
    /* PGD Base + RTS */
    /* RTS=0x05 for 52-bit VA? */
    
    entry->root_phys = (pml4_phys & 0x000FFFFFFFFFF000ULL) | RADIX_RTS_52;
    entry->process_control = 0;
    
    /* Invalidate TLB for this PID */
    /* TODO: tlbie pid */
    /* For Phase 0/1: Global invalidate is acceptable */
    /* asm_tlbie(); */
}
