/* AstraLisp OS Scheduler Implementation - Production Grade */

#include "scheduler.h"
#include "process.h"
#include "../asm/context-switch.h"
#include <stddef.h>
#include <stdbool.h>
#include <stdint.h>

#define MAX_PRIORITIES 5
#define TIME_SLICE_TICKS 10

/* Runqueues for each priority level */
static struct thread* runqueues[MAX_PRIORITIES] = {NULL};
static struct thread* current_thread = NULL;
static struct thread* idle_thread = NULL;
static uint64_t tick_count = 0;

/* Helper: Add to tail of runqueue */
static void enqueue_thread(struct thread* t) {
    if (!t) return;
    
    int prio = t->priority;
    if (prio < 0) prio = 0;
    if (prio >= MAX_PRIORITIES) prio = MAX_PRIORITIES - 1;
    
    t->state = THREAD_READY;
    t->next = NULL;
    
    if (!runqueues[prio]) {
        runqueues[prio] = t;
        t->prev = NULL;
    } else {
        struct thread* curr = runqueues[prio];
        while (curr->next) {
            curr = curr->next;
        }
        curr->next = t;
        t->prev = curr;
    }
}

/* Helper: Remove from runqueue */
static void dequeue_thread(struct thread* t) {
    if (!t) return;
    
    int prio = t->priority;
    if (prio < 0) prio = 0;
    if (prio >= MAX_PRIORITIES) prio = MAX_PRIORITIES - 1;
    
    if (t->prev) {
        t->prev->next = t->next;
    } else {
        runqueues[prio] = t->next;
    }
    
    if (t->next) {
        t->next->prev = t->prev;
    }
    
    t->next = NULL;
    t->prev = NULL;
}

/* Initialize scheduler */
int scheduler_init(void) {
    for (int i = 0; i < MAX_PRIORITIES; i++) {
        runqueues[i] = NULL;
    }
    current_thread = NULL;
    idle_thread = NULL; /* Should be created by caller */
    tick_count = 0;
    return 0;
}

/* Add thread to scheduler */
int scheduler_add_thread(void* thread) {
    struct thread* t = (struct thread*)thread;
    if (!t) return -1;
    
    t->time_slice = TIME_SLICE_TICKS;
    enqueue_thread(t);
    
    /* If no current thread, pick this one immediately */
    if (!current_thread) {
        current_thread = t;
        t->state = THREAD_RUNNING;
        dequeue_thread(t);
    }
    
    return 0;
}

/* Remove thread from scheduler */
int scheduler_remove_thread(void* thread) {
    struct thread* t = (struct thread*)thread;
    if (!t) return -1;
    
    if (t == current_thread) {
        /* Cannot remove running thread directly without yield */
        t->state = THREAD_ZOMBIE;
        scheduler_yield();
        return 0;
    }
    
    dequeue_thread(t);
    return 0;
}

/* Pick next thread */
static struct thread* pick_next_thread(void) {
    /* Search from highest priority down */
    for (int i = MAX_PRIORITIES - 1; i >= 0; i--) {
        if (runqueues[i]) {
            return runqueues[i];
        }
    }
    return idle_thread;
}

/* Yield to scheduler */
void scheduler_yield(void) {
    struct thread* prev = current_thread;
    struct thread* next = pick_next_thread();
    
    if (!next) {
        return; /* No threads to run */
    }
    
    if (prev && prev->state == THREAD_RUNNING) {
        /* Re-queue current thread if it was running */
        enqueue_thread(prev);
    }
    
    /* Dequeue next thread */
    if (next != idle_thread) {
        dequeue_thread(next);
    }
    
    next->state = THREAD_RUNNING;
    next->time_slice = TIME_SLICE_TICKS;
    current_thread = next;
    
    if (prev != next) {
        context_switch(&prev->context, &next->context);
    }
}

static struct thread* sleeping_threads = NULL;

/* Helper: Remove from sleep queue */
static void dequeue_sleep_thread(struct thread* t) {
    if (!t) return;
    
    if (t->prev) {
        t->prev->next = t->next;
    } else {
        sleeping_threads = t->next;
    }
    
    if (t->next) {
        t->next->prev = t->prev;
    }
    
    t->next = NULL;
    t->prev = NULL;
}

/* Helper: Add to sleep queue (sorted by wake time could be better, but simple list for now) */
static void enqueue_sleep_thread(struct thread* t) {
    if (!t) return;
    
    t->next = sleeping_threads;
    t->prev = NULL;
    
    if (sleeping_threads) {
        sleeping_threads->prev = t;
    }
    sleeping_threads = t;
}

/* Scheduler tick (called from timer interrupt) */
void scheduler_tick(void) {
    tick_count++;
    
    /* Wake up sleeping threads */
    struct thread* curr = sleeping_threads;
    while (curr) {
        struct thread* next = curr->next;
        
        if (tick_count >= curr->sleep_until) {
            /* Wake up */
            dequeue_sleep_thread(curr);
            curr->state = THREAD_READY;
            enqueue_thread(curr);
        }
        
        curr = next;
    }
    
    if (current_thread) {
        if (current_thread->time_slice > 0) {
            current_thread->time_slice--;
        }
        
        if (current_thread->time_slice == 0) {
            scheduler_yield();
        }
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
        current_thread->state = THREAD_SLEEPING;
        
        /* Add to sleep queue before yielding */
        enqueue_sleep_thread(current_thread);
        
        scheduler_yield();
    }
}

