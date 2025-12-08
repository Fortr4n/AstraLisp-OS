/* AstraLisp OS - Generic Wait Queues */

#ifndef WAIT_QUEUE_H
#define WAIT_QUEUE_H

#include "spinlock.h"
#include "../process/process.h"

/* Wait Queue Head */
typedef struct wait_queue {
    spinlock_t lock;
    struct thread* head;
    struct thread* tail;
} wait_queue_t;

/* Initialize wait queue */
void wait_queue_init(wait_queue_t* wq);

/* Sleep on wait queue (uninterruptible for now) */
/* Condition check is usually done by caller in a loop */
void wait_event(wait_queue_t* wq);

/* Wake up one thread */
void wake_up(wait_queue_t* wq);

/* Wake up all threads */
void wake_up_all(wait_queue_t* wq);

#endif
