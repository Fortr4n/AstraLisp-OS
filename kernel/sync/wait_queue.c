/* AstraLisp OS - Generic Wait Queues Implementation */

#include "wait_queue.h"
#include "../process/scheduler.h"
#include <stddef.h>

void wait_queue_init(wait_queue_t* wq) {
    if (!wq) return;
    spinlock_init(&wq->lock);
    wq->head = NULL;
    wq->tail = NULL;
}

void wait_event(wait_queue_t* wq) {
    if (!wq) return;
    
    struct thread* current = thread_get_current();
    if (!current) return;
    
    spinlock_acquire(&wq->lock);
    
    /* Add to queue */
    /* We reuse thread->next/prev ? No, thread->next is used by scheduler runqueues? 
       Threads in BLOCKED state are NOT in runqueues. 
       However, reusing next/prev requires careful handling if we use them for other lists.
       Ideally, we'd have a 'list_node' in struct thread. 
       For now, since BLOCKED threads are only in ONE wait queue, we can safely reuse next/prev.
    */
    
    current->state = THREAD_BLOCKED;
    current->next = NULL;
    
    if (!wq->head) {
        wq->head = current;
        wq->tail = current;
        current->prev = NULL;
    } else {
        wq->tail->next = current;
        current->prev = wq->tail;
        wq->tail = current;
    }
    
    spinlock_release(&wq->lock);
    
    /* Yield CPU */
    scheduler_yield();
}

void wake_up(wait_queue_t* wq) {
    if (!wq) return;
    
    spinlock_acquire(&wq->lock);
    
    struct thread* t = wq->head;
    if (t) {
        /* Remove from queue */
        wq->head = t->next;
        if (wq->head) {
            wq->head->prev = NULL;
        } else {
            wq->tail = NULL;
        }
        
        t->next = NULL;
        t->prev = NULL;
        
        /* Wake it up */
        scheduler_wake_thread(t);
    }
    
    spinlock_release(&wq->lock);
}

void wake_up_all(wait_queue_t* wq) {
    if (!wq) return;
    
    spinlock_acquire(&wq->lock);
    
    while (wq->head) {
        struct thread* t = wq->head;
        wq->head = t->next;
        
        t->next = NULL;
        t->prev = NULL;
        
        scheduler_wake_thread(t);
    }
    wq->tail = NULL;
    
    spinlock_release(&wq->lock);
}
