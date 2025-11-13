/* AstraLisp OS Scheduler */

#ifndef SCHEDULER_H
#define SCHEDULER_H

#include <stdint.h>
#include <stdbool.h>

/* Initialize scheduler */
int scheduler_init(void);

/* Add thread to scheduler */
int scheduler_add_thread(void* thread);

/* Remove thread from scheduler */
int scheduler_remove_thread(void* thread);

/* Yield to scheduler */
void scheduler_yield(void);

/* Scheduler tick (called from timer interrupt) */
void scheduler_tick(void);

/* Get current thread */
void* scheduler_get_current_thread(void);

/* Sleep for ticks */
void scheduler_sleep(uint32_t ticks);

#endif /* SCHEDULER_H */
