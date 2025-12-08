/* AstraLisp OS - Sleeping Mutex */

#ifndef MUTEX_H
#define MUTEX_H

#include "spinlock.h"
#include "../process/process.h"

/* Mutex structure */
typedef struct mutex {
    spinlock_t lock;           /* Protects the mutex state */
    struct thread* owner;      /* Current owner or NULL */
    struct thread* wait_queue; /* Queue of blocked threads */
} mutex_t;

/* Initialize mutex */
void mutex_init(mutex_t* mutex);

/* Acquire mutex (blocks if unavailable) */
void mutex_acquire(mutex_t* mutex);

/* Release mutex (wakes up waiter) */
void mutex_release(mutex_t* mutex);

#endif
