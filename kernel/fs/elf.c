/* AstraLisp OS - ELF64 Loader Implementation */

#include "elf.h"
#include "../../filesystem/lfsx/lfsx.h"
#include "../mm/vmm.h"
#include "../mm/pmm.h"
#include "../mm/heap.h"
#include "../process/process.h"
#include <stddef.h>
#include <string.h>

/* VMM Flags Mapping */
#define VMM_USER    0x004
#define VMM_WRITE   0x002
#define VMM_PRESENT 0x001
#define VMM_EXEC    0x000 /* NX bit logic depends on VMM, assume exec by default */

/* Page Size */
#define PAGE_SIZE 4096

/* Verify ELF Magic */
static int elf_verify_header(Elf64_Ehdr* ehdr) {
    if (ehdr->e_ident[EI_MAG0] != ELFMAG0 ||
        ehdr->e_ident[EI_MAG1] != ELFMAG1 ||
        ehdr->e_ident[EI_MAG2] != ELFMAG2 ||
        ehdr->e_ident[EI_MAG3] != ELFMAG3) {
        return -1; /* Bad Magic */
    }
    
    /* Check Class (64-bit) */
    if (ehdr->e_ident[EI_CLASS] != 2) { /* ELFCLASS64 */
        return -2;
    }
    
    /* Check Endianness (Little) */
    if (ehdr->e_ident[EI_DATA] != 1) { /* ELFDATA2LSB */
        return -3;
    }
    
    /* Check Machine Type */
    if (ehdr->e_machine != EM_PPC64) {
        return -4;
    }
    
    /* Check Type (Executable) */
    if (ehdr->e_type != 2) { /* ET_EXEC */
        return -5;
    }
    
    return 0;
}

/* Map ELF flags to VMM flags */
static uint32_t elf_flags_to_vmm(Elf64_Word p_flags) {
    uint32_t vmm_flags = VMM_PRESENT | VMM_USER;
    
    if (p_flags & PF_W) {
        vmm_flags |= VMM_WRITE;
    }
    /* PF_X handling: VMM may use NX bit. For simplicity, we allow exec always for now. */
    
    return vmm_flags;
}

/* Load a single segment */
static int elf_load_segment(struct process* proc, struct lfsx_file* file,
                             Elf64_Phdr* phdr, uint8_t* file_buffer) {
    if (phdr->p_type != PT_LOAD) {
        return 0; /* Skip non-loadable segments */
    }
    
    uintptr_t vaddr = phdr->p_vaddr;
    size_t memsz = phdr->p_memsz;
    size_t filesz = phdr->p_filesz;
    size_t file_offset = phdr->p_offset;
    
    /* Align to page boundaries */
    uintptr_t page_start = vaddr & ~(PAGE_SIZE - 1);
    uintptr_t page_end = (vaddr + memsz + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1);
    size_t num_pages = (page_end - page_start) / PAGE_SIZE;
    
    uint32_t vmm_flags = elf_flags_to_vmm(phdr->p_flags);
    
    /* Allocate and map pages */
    for (size_t i = 0; i < num_pages; i++) {
        uintptr_t page_vaddr = page_start + (i * PAGE_SIZE);
        
        /* Allocate physical frame */
        uintptr_t frame = pmm_alloc();
        if (!frame) {
            return -1; /* Out of memory */
        }
        
        /* Map into process address space */
        if (vmm_map_page(proc->page_directory, page_vaddr, frame, vmm_flags) != 0) {
            pmm_free(frame);
            return -2;
        }
        
        /* Zero the page initially (for BSS and partial pages) */
        /* We need to map it temporarily to kernel space to write */
        /* Simpler: Use a kernel identity mapping assumption or vmm helper */
        /* For now, assume identity mapped or use direct pointer */
        memset((void*)frame, 0, PAGE_SIZE);
    }
    
    /* Copy file content to mapped pages */
    if (filesz > 0 && file_buffer) {
        /* Calculate offset within first page */
        size_t page_offset = vaddr - page_start;
        
        /* The physical address of first page */
        /* We need to resolve vaddr -> phys for writing */
        /* Assuming identity map for kernel access: */
        uintptr_t phys_base = vmm_get_phys(proc->page_directory, page_start);
        if (phys_base) {
            uint8_t* dest = (uint8_t*)phys_base + page_offset;
            memcpy(dest, file_buffer + file_offset, filesz);
        }
    }
    
    return 0;
}

/* Load ELF from file path into process */
int elf_load(struct process* proc, const char* path, uintptr_t* entry_point) {
    if (!proc || !path || !entry_point) {
        return -1;
    }
    
    /* Open ELF file */
    struct lfsx_file* file = lfsx_open(path, 0);
    if (!file) {
        return -2; /* File not found */
    }
    
    /* Read entire file into buffer (for simplicity, limit to 1MB) */
    /* Production: mmap or read segments on demand */
    #define MAX_ELF_SIZE (1024 * 1024)
    uint8_t* buffer = (uint8_t*)kmalloc(MAX_ELF_SIZE);
    if (!buffer) {
        lfsx_close(file);
        return -3;
    }
    
    size_t bytes_read = lfsx_read(file, buffer, MAX_ELF_SIZE);
    lfsx_close(file);
    
    if (bytes_read < sizeof(Elf64_Ehdr)) {
        kfree(buffer);
        return -4; /* File too small */
    }
    
    /* Parse ELF Header */
    Elf64_Ehdr* ehdr = (Elf64_Ehdr*)buffer;
    
    int verify = elf_verify_header(ehdr);
    if (verify != 0) {
        kfree(buffer);
        return verify;
    }
    
    /* Verify Program Headers exist */
    if (ehdr->e_phoff == 0 || ehdr->e_phnum == 0) {
        kfree(buffer);
        return -6;
    }
    
    /* Load each segment */
    Elf64_Phdr* phdrs = (Elf64_Phdr*)(buffer + ehdr->e_phoff);
    
    for (Elf64_Half i = 0; i < ehdr->e_phnum; i++) {
        Elf64_Phdr* phdr = &phdrs[i];
        
        int result = elf_load_segment(proc, file, phdr, buffer);
        if (result != 0) {
            kfree(buffer);
            return result;
        }
    }
    
    /* Set entry point */
    *entry_point = ehdr->e_entry;
    
    kfree(buffer);
    return 0;
}

/* Load ELF from memory buffer */
int elf_load_from_memory(struct process* proc, const void* data, size_t size, 
                          uintptr_t* entry_point) {
    if (!proc || !data || size < sizeof(Elf64_Ehdr) || !entry_point) {
        return -1;
    }
    
    const uint8_t* buffer = (const uint8_t*)data;
    Elf64_Ehdr* ehdr = (Elf64_Ehdr*)buffer;
    
    int verify = elf_verify_header(ehdr);
    if (verify != 0) {
        return verify;
    }
    
    if (ehdr->e_phoff == 0 || ehdr->e_phnum == 0) {
        return -6;
    }
    
    Elf64_Phdr* phdrs = (Elf64_Phdr*)(buffer + ehdr->e_phoff);
    
    for (Elf64_Half i = 0; i < ehdr->e_phnum; i++) {
        Elf64_Phdr* phdr = &phdrs[i];
        
        if (phdr->p_type != PT_LOAD) continue;
        
        uintptr_t vaddr = phdr->p_vaddr;
        size_t memsz = phdr->p_memsz;
        size_t filesz = phdr->p_filesz;
        size_t file_offset = phdr->p_offset;
        
        uintptr_t page_start = vaddr & ~(PAGE_SIZE - 1);
        uintptr_t page_end = (vaddr + memsz + PAGE_SIZE - 1) & ~(PAGE_SIZE - 1);
        size_t num_pages = (page_end - page_start) / PAGE_SIZE;
        
        uint32_t vmm_flags = elf_flags_to_vmm(phdr->p_flags);
        
        for (size_t j = 0; j < num_pages; j++) {
            uintptr_t page_vaddr = page_start + (j * PAGE_SIZE);
            uintptr_t frame = pmm_alloc();
            if (!frame) return -7;
            
            if (vmm_map_page(proc->page_directory, page_vaddr, frame, vmm_flags) != 0) {
                pmm_free(frame);
                return -8;
            }
            
            memset((void*)frame, 0, PAGE_SIZE);
        }
        
        if (filesz > 0) {
            size_t page_offset = vaddr - page_start;
            uintptr_t phys_base = vmm_get_phys(proc->page_directory, page_start);
            if (phys_base) {
                uint8_t* dest = (uint8_t*)phys_base + page_offset;
                memcpy(dest, buffer + file_offset, filesz);
            }
        }
    }
    
    *entry_point = ehdr->e_entry;
    return 0;
}
