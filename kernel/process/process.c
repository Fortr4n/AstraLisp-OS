/* AstraLisp OS Process and Thread Implementation */

#include "process.h"
#include "pmm.h"
#include "vmm.h"
#include "../mm/heap.h"
#include <stddef.h>
#include <string.h>

uint32_t next_pid = 1;
uint32_t next_tid = 1;
struct process* process_list = NULL;
struct process* current_process = NULL;

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
    
    if (!current_process) {
        current_process = proc;
    }
    
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
    
    if (current_process == proc) {
        current_process = process_list;
    }
    
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

/* Get current process */
struct process* process_get_current(void) {
    return current_process;
}

/* Get current thread */
struct thread* thread_get_current(void) {
    if (current_process) {
        return current_process->threads;
    }
    return NULL;
}
