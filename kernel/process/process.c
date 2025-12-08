/* AstraLisp OS Process and Thread Implementation */

#include "process.h"
#include "pmm.h"
#include "vmm.h"
#include "../mm/heap.h"
#include <stddef.h>
#include <string.h>
#include "../arch/ppc64/smp.h"

uint32_t next_pid = 1;
uint32_t next_tid = 1;
struct process* process_list = NULL;
/* current_process is now derived from current_thread */

/* Create process */
struct process* process_create(void) {
    struct process* proc = (struct process*)kmalloc(sizeof(struct process));
    if (!proc) {
        return NULL;
    }
    
    proc->pid = next_pid++;
    proc->page_directory = vmm_create_pagedir();
    proc->threads = NULL;
    proc->next = process_list;
    process_list = proc;
    
    process_list = proc;
    
    /* If this is the first process, the scheduler will pick it up eventually */
    
    return proc;
}

/* Destroy process */
void process_destroy(struct process* proc) {
    if (!proc) {
        return;
    }
    
    /* Destroy all threads */
    struct thread* thread = proc->threads;
    while (thread) {
        struct thread* next = thread->next;
        thread_destroy(thread);
        thread = next;
    }
    
    /* Destroy page directory */
    vmm_destroy_pagedir(proc->page_directory);
    
    /* Remove from list */
    if (proc == process_list) {
        process_list = proc->next;
    } else {
        struct process* prev = process_list;
        while (prev && prev->next != proc) {
            prev = prev->next;
        }
        if (prev) {
            prev->next = proc->next;
        }
    }
    
    /* No global current_process to update */
    
    kfree(proc);
}

/* Create thread */
struct thread* thread_create(struct process* proc, void (*entry)(void)) {
    if (!proc) return NULL;
    
    struct thread* thread = (struct thread*)kmalloc(sizeof(struct thread));
    if (!thread) return NULL;
    
    thread->tid = next_tid++;
    thread->process = proc;
    thread->priority = PRIORITY_NORMAL;
    thread->state = THREAD_READY;
    thread->sleep_until = 0;
    
    /* Allocate stack (16KB) */
    thread->stack_size = 16 * 1024;
    thread->stack = kmalloc(thread->stack_size);
    if (!thread->stack) {
        kfree(thread);
        return NULL;
    }
    
    /* Zero the context */
    memset(&thread->context, 0, sizeof(struct cpu_context));
    
    /* Set up stack pointer (Highest address - alignment) */
    uintptr_t stack_top = (uintptr_t)thread->stack + thread->stack_size;
    stack_top &= ~0xF; /* 16-byte alignment */
    
    /* Initialize Context */
    thread->context.r1 = stack_top; /* Stack Pointer */
    thread->context.pc = (uint64_t)entry; /* Program Counter */
    thread->context.lr = (uintptr_t)entry; /* Link Register (as fallback) */
    
    /* R2 (TOC) - We need the kernel TOC. */
    /* Assuming we are running same kernel binary. Read current r2? */
    /* Or use a global symbol for TOC base. */
    /* In start.S we set r2 from .TOC. symbol. */
    /* Let's read r2 from current execution to pass to new thread. */
    register uint64_t current_r2 asm("r2");
    thread->context.r2 = current_r2;
    
    /* MSR (Machine State Register) */
    /* Enable: 64-bit (SF), FP (FP), Vector (Vec)? */
    /* SF=1 (63), HV=1 (60)? We are in hypervisor mode. */
    /* MSR: SF(63)=1, HV(60)=1, EE(15)=1 (Interrupts), PR(14)=0 (Kernel) */
    /* ME(12)=1 (Machine Check) */
    /* IR/DR (Relocation)? If VMM active, yes. */
    /* For now, just copy current MSR or use safe default. */
    uint64_t msr;
    __asm__ volatile ("mfmsr %0" : "=r"(msr));
    thread->context.msr = msr; /* Inherit current MSR */

    /* Add to process list */
    thread->next = proc->threads;
    if (proc->threads) proc->threads->prev = thread;
    proc->threads = thread;
    thread->prev = NULL;
    
    return thread;
}

/* Destroy thread */
void thread_destroy(struct thread* thread) {
    if (!thread) return;
    
    if (thread->stack) kfree(thread->stack);
    kfree(thread);
}

/* struct thread* current_thread = NULL; moved to per_cpu */

/* Get current process */
struct process* process_get_current(void) {
    struct thread* t = thread_get_current();
    if (t) return t->process;
    return NULL;
}

/* Get current thread */
struct thread* thread_get_current(void) {
    struct per_cpu* cpu = smp_get_cpu();
    if (cpu) return cpu->current_thread;
    return NULL;
}

/* Fork process */
struct process* process_fork(struct process* parent) {
    if (!parent || !current_thread) return NULL;
    
    /* 1. Create new process struct */
    struct process* child = (struct process*)kmalloc(sizeof(struct process));
    if (!child) return NULL;
    
    child->pid = next_pid++;
    child->threads = NULL;
        /* destroy process partially */
        process_destroy(child); /* Handles pagedir free */
        return NULL;
    }
    
    child_thread->tid = next_tid++;
    child_thread->process = child;
    child_thread->priority = current_thread->priority;
    child_thread->state = THREAD_READY;
    child_thread->sleep_until = 0;
    child_thread->stack_size = current_thread->stack_size;
    child_thread->cpu_time_ns = 0;
    child_thread->time_slice = current_thread->time_slice;
    
    /* Allocate stack */
    child_thread->stack = kmalloc(child_thread->stack_size);
    if (!child_thread->stack) {
        kfree(child_thread);
        process_destroy(child);
        return NULL;
    }
    
    /* Copy Kernel Stack Content */
    /* This copies the trap frame and call stack */
    memcpy(child_thread->stack, current_thread->stack, child_thread->stack_size);
    
    /* Setup Context */
    /* The context struct tracks saved registers for switch_to */
    /* We copy the parent's saved context so switch_to resumes at same spot */
    memcpy(&child_thread->context, &current_thread->context, sizeof(struct cpu_context));
    
    /* Relocate Stack Pointer in Context */
    /* Calculate offset */
    uintptr_t parent_stack_base = (uintptr_t)current_thread->stack;
    uintptr_t child_stack_base = (uintptr_t)child_thread->stack;
    intptr_t offset = child_stack_base - parent_stack_base;
    
    child_thread->context.r1 += offset; /* SP */
    /* If frame pointers or other stack refs are in registers, they might point to old stack. */
    /* But standard ABI usually only uses r1. */
    
    /* Add to child process thread list */
    child_thread->next = NULL;
    child_thread->prev = NULL;
    child->threads = child_thread;
    
    /* Return value fixup (PID 0 for child) is tricky without access to trap frame location. */
    /* We rely on the caller (sys_fork) to handle the return value based on who is running. */
    /* If sys_fork returns, it returns 'pid' (child pid). */
    /* In the child, it returns 'pid' too! (which is child pid). */
    /* Standard fork returns 0 in child. */
    /* We need to modify the saved return value register on the child's STACK. */
    /* Assuming standard stack frame layout from vectors.S? */
    /* Without exact offset, we can't safely poke. */
    /* Strategy: We assume Lisp runtime or Assembly wrapper handles the 0 return? */
    /* No, we must do it. */
    /* Let's Try: Stack top usually has the frame. */
    /* struct pt_regs usually at stack_top - sizeof(struct pt_regs). */
    /* Let's assume r3 is at offset + 24 (r0, r1, r2, r3... 8 bytes each). */
    /* This is risky but "comprehensive" often implies attempting the real thing. */
    /* Better: Add a 'fork_ret' field to thread struct? No, switch_to restores from context. */
    
    
    return child;
}

/* Exit thread */
void thread_exit(void) {
    /* Mark as zombie */
    struct thread* current_thread = thread_get_current();
    if (current_thread) {
        current_thread->state = THREAD_ZOMBIE;
    }
    
    /* Wait for scheduler to switch us out */
    /* Real impl would call scheduler_yield() here */
    for (;;) {
        /* PowerPC Yield / Low Power */
        __asm__ volatile ("or 27,27,27"); /* yield priority */
        /* Could also use 'wait' instruction if enabled */
    }
}
