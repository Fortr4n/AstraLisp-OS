/* AstraLisp OS Threading System
 * 
 * Per-thread Lisp execution contexts with kernel thread integration.
 */

#ifndef THREAD_H
#define THREAD_H

#include "tagged.h"
#include "vm.h"
#include <stdint.h>
#include <stdbool.h>

/* Thread configuration */
#define MAX_LISP_THREADS 64

/* ========== Thread States ========== */

typedef enum {
    THREAD_CREATED,     /* Thread created but not started */
    THREAD_RUNNING,     /* Currently executing */
    THREAD_BLOCKED,     /* Waiting for I/O, lock, etc. */
    THREAD_COMPLETED,   /* Finished execution */
    THREAD_FAILED       /* Exception during execution */
} thread_state_t;

/* ========== Thread Structure ========== */

struct lisp_thread {
    /* Identification */
    uint64_t id;
    char name[64];
    
    /* Execution context */
    struct vm_context vm;           /* Full VM state per-thread */
    lisp_value function;            /* Function to execute */
    lisp_value result;              /* Result after completion */
    lisp_value error;               /* Error if failed */
    
    /* State */
    thread_state_t state;
    bool detached;                  /* If true, resources freed on completion */
    
    /* Synchronization */
    bool join_requested;            /* Another thread waiting to join */
    struct lisp_thread* waiter;     /* Thread waiting on this one */
    
    /* Kernel integration */
#ifdef KERNEL
    void* kernel_thread;            /* Kernel thread handle */
    void* kernel_stack;             /* Kernel stack allocation */
#else
    void* native_handle;            /* Platform thread handle (pthread_t, etc.) */
#endif
    
    /* GC integration */
    bool gc_safe_point;             /* At safe point for GC */
    struct gc_stack_frame* gc_stack; /* Thread's GC stack */
    
    /* Linked list for thread registry */
    struct lisp_thread* next;
    struct lisp_thread* prev;
};

/* ========== Thread Registry ========== */

struct thread_registry {
    struct lisp_thread* threads;    /* Linked list of all threads */
    uint64_t thread_count;
    uint64_t next_id;
    
    /* Main thread */
    struct lisp_thread* main_thread;
    
    /* Current thread (thread-local) */
    /* In kernel, stored in CPU context; otherwise pthread_key */
};

/* ========== Thread API ========== */

/* Initialize threading system */
int thread_init(void);

/* Shutdown threading system */
void thread_shutdown(void);

/* Get current thread */
struct lisp_thread* thread_current(void);

/* Create new thread */
struct lisp_thread* thread_create(lisp_value function, const char* name);

/* Start thread execution */
int thread_start(struct lisp_thread* thread);

/* Wait for thread completion */
lisp_value thread_join(struct lisp_thread* thread);

/* Detach thread (auto-cleanup on completion) */
void thread_detach(struct lisp_thread* thread);

/* Yield execution to other threads */
void thread_yield(void);

/* Set thread to GC safe point */
void thread_enter_safe_point(void);
void thread_leave_safe_point(void);

/* ========== Stop-the-World GC Support ========== */

/* Request all threads stop at safe points */
void threads_request_stop(void);

/* Wait for all threads to be stopped */
void threads_wait_stopped(void);

/* Resume all threads */
void threads_resume(void);

/* Iterate over all thread roots for GC */
void threads_mark_roots(void);

/* ========== Built-in Functions ========== */

/* (spawn fn) - Create and start new thread */
lisp_value builtin_spawn(lisp_value env, lisp_value args);

/* (join thread) - Wait for thread completion */
lisp_value builtin_join(lisp_value env, lisp_value args);

/* (thread-yield) - Yield to other threads */
lisp_value builtin_thread_yield(lisp_value env, lisp_value args);

/* (current-thread) - Get current thread as Lisp object */
lisp_value builtin_current_thread(lisp_value env, lisp_value args);

/* (thread?) - Check if value is a thread */
lisp_value builtin_threadp(lisp_value env, lisp_value args);

/* (thread-state thread) - Get thread state as symbol */
lisp_value builtin_thread_state(lisp_value env, lisp_value args);

#endif /* THREAD_H */
