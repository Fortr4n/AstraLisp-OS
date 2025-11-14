/* AstraLisp OS Process and Thread Management */

#ifndef PROCESS_H
#define PROCESS_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* Process structure */
struct process {
    uint32_t pid;
    void* page_directory;
    struct thread* threads;
    struct process* next;
};

/* Thread structure */
struct thread {
    uint32_t tid;
    struct process* process;
    void* stack;
    void* context;
    uint32_t sleep_until;
    struct thread* next;
};

/* Create process */
struct process* process_create(void);

/* Destroy process */
void process_destroy(struct process* proc);

/* Create thread */
struct thread* thread_create(struct process* proc, void (*entry)(void));

/* Destroy thread */
void thread_destroy(struct thread* thread);

/* Get current process */
struct process* process_get_current(void);

/* Get current thread */
struct thread* thread_get_current(void);

#endif /* PROCESS_H */
