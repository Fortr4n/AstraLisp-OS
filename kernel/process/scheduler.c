/* AstraLisp OS Scheduler Implementation */

#include "scheduler.h"
#include "process.h"
#include "../asm/context-switch.h"
#include <stddef.h>
#include <stdbool.h>

static struct thread* current_thread = NULL;
static struct thread* thread_list = NULL;
static uint32_t tick_count = 0;

/* Initialize scheduler */
int scheduler_init(void) {
    current_thread = NULL;
    thread_list = NULL;
    tick_count = 0;
    return 0;
}

/* Add thread to scheduler */
int scheduler_add_thread(void* thread) {
    struct thread* t = (struct thread*)thread;
    
    if (!t) {
        return -1;
    }
    
    t->next = thread_list;
    thread_list = t;
    
    if (!current_thread) {
        current_thread = t;
    }
    
    return 0;
}

/* Remove thread from scheduler */
int scheduler_remove_thread(void* thread) {
    struct thread* t = (struct thread*)thread;
    
    if (!t) {
        return -1;
    }
    
    if (t == thread_list) {
        thread_list = t->next;
    } else {
        struct thread* prev = thread_list;
        while (prev && prev->next != t) {
            prev = prev->next;
        }
        if (prev) {
            prev->next = t->next;
        }
    }
    
    if (current_thread == t) {
        current_thread = thread_list;
    }
    
    return 0;
}

/* Yield to scheduler */
void scheduler_yield(void) {
    if (!thread_list || !thread_list->next) {
        return;  /* Only one thread or no threads */
    }
    
    /* Find next thread */
    struct thread* next = current_thread->next;
    if (!next) {
        next = thread_list;
    }
    
    /* Switch context */
    context_switch(&current_thread->context, &next->context);
    current_thread = next;
}

/* Scheduler tick */
void scheduler_tick(void) {
    tick_count++;
    
    /* Round-robin scheduling */
    if (thread_list && thread_list->next) {
        scheduler_yield();
    }
}

/* Get current thread */
void* scheduler_get_current_thread(void) {
    return current_thread;
}

/* Sleep for ticks */
void scheduler_sleep(uint32_t ticks) {
    if (current_thread) {
        current_thread->sleep_until = tick_count + ticks;
        scheduler_yield();
    }
}
