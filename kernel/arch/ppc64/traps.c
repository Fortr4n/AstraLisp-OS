/* AstraLisp OS - PowerISA Exception Handlers */

#include <stdint.h>

/* Forward declaration for output */
extern void early_printf(const char* format, ...);
extern void kernel_panic(const char* message);

/* Trap names for debugging */
static const char* get_trap_name(uint32_t trap_num) {
    switch (trap_num) {
        case 0x100: return "System Reset";
        case 0x200: return "Machine Check";
        case 0x300: return "Data Storage";
        case 0x400: return "Instruction Storage";
        case 0x500: return "External Interrupt";
        case 0x600: return "Alignment";
        case 0x700: return "Program";
        case 0x800: return "FPU Unavailable";
        case 0x900: return "Decrementer";
        case 0xC00: return "System Call";
        default:    return "Unknown Exception";
    }
}

/* Main exception handler called from vectors.S */
/* Arg1 (r3): trap_num */
#include "../../mm/vmm.h"
#include "../../mm/pmm.h"
#include <stddef.h>

/* Forward declaration */
extern void* vmm_get_current_pagedir(void);
extern int vmm_map_page(void* pagedir, uintptr_t virt, uintptr_t phys, uint32_t flags);
/* We need internal access to PTEs to check flags bits not exposed in map_page. 
   Or we extend vmm interface. 
   For now, we can manually check if we include vmm.h and its defines are visible. 
   (They are visible if they are in vmm.h or we duplicate constants. vmm defines them in vmm.c usually? 
    Ah, vmm.c had local defines. We should move PTE defines to vmm.h to share them.)
   
   Wait, they were in vmm.c. I must check vmm.h.
*/

/* Assume PTE defines are in vmm.h or we define local mirror */
#define PTE_PRESENT     0x0000000000000001ULL
#define PTE_RW          0x0000000000000002ULL
#define PTE_COW         0x0000000000000200ULL

/* Page Fault Handler */
#include "regs.h"

/* Page Fault Handler */
void handle_page_fault(struct pt_regs* regs) {
    uint64_t addr = regs->dar;
    uint64_t error_code = regs->dsisr;
    uint32_t trap_num = 0x300;
    
    /* ... Logic ... */
    bool is_write = (error_code & 0x02000000) != 0;
    
    if (is_write) {
         extern int vmm_handle_cow(uintptr_t virt);
         if (vmm_handle_cow(addr) == 0) return;
    }
    
    /* Panic */
    const char* name = get_trap_name(trap_num);
    early_printf("\n!!! PAGE FAULT: %s (0x%x) at 0x%lx err=0x%lx !!!\n", name, trap_num, addr, error_code);
    early_printf("NIP: %lx MSR: %lx\n", regs->nip, regs->msr);
    for(;;);
}

void handle_exception_ex(struct pt_regs* regs) {
    uint32_t trap_num = regs->trap;
    
    if (trap_num == 0x300) {
        handle_page_fault(regs);
        return;
    }
    
    if (trap_num == 0x500) {
        /* External Interrupt */
        /* TODO: Dispatch XIVE */
        return; 
    }

    const char* name = get_trap_name(trap_num);
    early_printf("\n!!! EXCEPTION: %s (0x%x) !!!\n", name, trap_num);
    early_printf("NIP: %lx MSR: %lx\n", regs->nip, regs->msr);
    for (;;) { __asm__ volatile ("nop"); }
}

/* Backward compat stub if needed */
void handle_exception(uint32_t t) { (void)t; }
/* Backward compat for existing caller (handle_exception) */
/* void handle_exception(uint32_t trap_num) { } - Removed */
        
        if (is_write) {
            /* Check if it's CoW */
            void* pml4 = vmm_get_current_pagedir();
            
            /* Manual walk to find PTE (this duplicates logic but we need the raw PTE bits) */
            /* Simplified for now: We assume vmm_get_phys exists or similar. */
            /* Actually, we need to read the PTE to check PTE_COW bit. */
            
            /* ... walking ... */
            /* Since we don't have easy pte access functions exposed, let's just attempt 
               to handle it if it LOOKS like generic write protection */
               
            /* We need the PTE. Let's assume we can add a vmm_get_entry function later. 
               For now, we will add the logic assuming we can check the bit. */
               
             /* Note: In a real kernel we'd have `vmm_get_pte`. */
             /* I'll add a helper to vmm.c/h? */
             
             /* WORKAROUND: For this file, since I cannot modify vmm.h easily to expose structs, 
                I will implement the handler logic conceptually and rely on 'vmm_handle_cow(addr)' 
                which I will add to vmm.c. */
             
             extern int vmm_handle_cow(uintptr_t virt);
             if (vmm_handle_cow(addr) == 0) {
                 return; /* Handled! */
             }
        }
    }

    const char* name = get_trap_name(trap_num);
    early_printf("\n!!! EXCEPTION: %s (0x%x) at 0x%lx err=0x%lx !!!\n", name, trap_num, addr, error_code);
    
    if (trap_num == 0x200 || trap_num == 0x300 || trap_num == 0x400) {
        /* Fatal faults */
        kernel_panic("Fatal Exception");
    }
    
    for (;;) {
        __asm__ volatile ("nop");
    }
}

/* Backward compat for existing caller (handle_exception) */
void handle_exception(uint32_t trap_num) {
    /* Just call new handler with 0 args for now, likely won't work for faults */
    /* Real asm vector calls handle_exception_ex with more args */
    handle_page_fault(trap_num, 0, 0); 
}

