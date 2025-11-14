/* AstraLisp OS Process and Thread Implementation */

#include "process.h"
#include "pmm.h"
#include "vmm.h"
#include "../mm/heap.h"
#include <stddef.h>
#include <string.h>

static uint32_t next_pid = 1;
static uint32_t next_tid = 1;
static struct process* process_list = NULL;
static struct process* current_process = NULL;

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
    if (!proc) {
        return NULL;
    }
    
    struct thread* thread = (struct thread*)kmalloc(sizeof(struct thread));
    if (!thread) {
        return NULL;
    }
    
    thread->tid = next_tid++;
    thread->process = proc;
    thread->sleep_until = 0;
    
    /* Allocate stack */
    thread->stack = kmalloc(8192);  /* 8KB stack */
    if (!thread->stack) {
        kfree(thread);
        return NULL;
    }
    
    /* Allocate context */
    thread->context = kmalloc(816);  /* Context size from context-switch.asm */
    if (!thread->context) {
        kfree(thread->stack);
        kfree(thread);
        return NULL;
    }
    
    /* Set up initial context */
    /* Stack pointer */
    uintptr_t* stack = (uintptr_t*)thread->stack;
    stack += 1024;  /* Top of stack */
    *((uintptr_t*)thread->context + 1) = (uintptr_t)stack;  /* r1 = stack pointer */
    
    /* Entry point */
    *((uintptr_t*)thread->context + 32) = (uintptr_t)entry;  /* LR = entry point */
    
    /* Add to process */
    thread->next = proc->threads;
    proc->threads = thread;
    
    return thread;
}

/* Destroy thread */
void thread_destroy(struct thread* thread) {
    if (!thread) {
        return;
    }
    
    kfree(thread->context);
    kfree(thread->stack);
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
