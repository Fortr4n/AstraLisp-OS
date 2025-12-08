/* AstraLisp OS - Sleeping Mutex Implementation */

#include "mutex.h"
#include "../process/scheduler.h"
#include "../process/process.h" /* For struct thread access if needed, though mutex.h includes it */
#include <stddef.h>

void mutex_init(mutex_t* mutex) {
    if (!mutex) return;
    spinlock_init(&mutex->lock);
    mutex->owner = NULL;
    mutex->wait_queue = NULL;
}

void mutex_acquire(mutex_t* mutex) {
    if (!mutex) return;

    struct thread* current = thread_get_current();
    if (!current) return; /* Should panic in kernel */

    for (;;) {
        spinlock_acquire(&mutex->lock);
        
        if (mutex->owner == NULL) {
            /* Available */
            mutex->owner = current;
            spinlock_release(&mutex->lock);
            return;
        }
        
        /* Contention: Add to wait queue and sleep */
        
        /* Check if already in wait queue? 
           Simplest list add: */
        current->next = mutex->wait_queue; /* Re-purposing next pointer? 
           WAIT! 'next' is used by scheduler runqueues! 
           Use 'prev' for wait queues? Or generic list node?
           Struct thread has next/prev. 
           If thread is BLOCKED, it is NOT in runqueue, so next/prev are free. */
        
        mutex->wait_queue = current;
        current->state = THREAD_BLOCKED;
        
        spinlock_release(&mutex->lock);
        
        /* Yield CPU */
        scheduler_yield();
        
        /* Upon return, we loop and try again. 
           (MESA monitor semantics, or strict handoff?) 
           If strict handoff, owner would be set by releaser. 
           Let's use loop for safety. */
    }
}

void mutex_release(mutex_t* mutex) {
    if (!mutex) return;
    
    spinlock_acquire(&mutex->lock);
    
    struct thread* current = thread_get_current();
    if (mutex->owner != current) {
        /* Error: release checks */
        spinlock_release(&mutex->lock);
        return;
    }
    
    mutex->owner = NULL;
    
    /* Wake up one waiter */
    if (mutex->wait_queue) {
        struct thread* next_thread = mutex->wait_queue;
        mutex->wait_queue = next_thread->next; /* Pop */
        
        next_thread->next = NULL; /* Clean up links */
        /* Note: scheduler_wake_thread will handle setting state to READY and enqueueing */
        scheduler_wake_thread(next_thread);
    }
    
    spinlock_release(&mutex->lock);
}
