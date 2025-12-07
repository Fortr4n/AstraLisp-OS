/* AstraLisp OS - Spinlock Synchronization */

#ifndef _KERNEL_SPINLOCK_H
#define _KERNEL_SPINLOCK_H

#include <stdint.h>
#include <stdbool.h>

typedef struct {
    volatile int lock; /* 0 = unlocked, 1 = locked */
} spinlock_t;

void spinlock_init(spinlock_t* lock);
void spinlock_acquire(spinlock_t* lock);
void spinlock_release(spinlock_t* lock);

#endif
