/* AstraLisp OS - Spinlock Implementation */

#include "spinlock.h"

void spinlock_init(spinlock_t* lock) {
    lock->lock = 0;
}

void spinlock_acquire(spinlock_t* lock) {
    while (__atomic_test_and_set(&lock->lock, __ATOMIC_ACQUIRE)) {
        /* Spin wait loop hint */
        /* PowerISA: "or 27,27,27" is yield/priority low */
        __asm__ volatile("or 27,27,27");
    }
}

void spinlock_release(spinlock_t* lock) {
    __atomic_clear(&lock->lock, __ATOMIC_RELEASE);
}
