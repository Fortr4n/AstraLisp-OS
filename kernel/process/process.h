/* AstraLisp OS Process and Thread Management */

#ifndef PROCESS_H
#define PROCESS_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* Process structure */
#include "../sync/mutex.h"
#include "../sync/wait_queue.h"

/* IPC Message */
struct message {
    uint32_t sender_pid;
    uint32_t length;
    uint8_t data[256];
    struct message* next;
};

/* Process structure */
struct process {
    uint32_t pid;
    void* page_directory;
    struct thread* threads;
    uint64_t cpu_time_ns;
    
    /* IPC */
    struct message* msg_queue_head;
    struct message* msg_queue_tail;
    mutex_t msg_lock;
    wait_queue_t msg_wait;
    
    struct process* next;
};

#include "../asm/context-switch.h"

/* Priority levels */
enum thread_priority {
    PRIORITY_IDLE = 0,
    PRIORITY_LOW = 1,
    PRIORITY_NORMAL = 2,
    PRIORITY_HIGH = 3,
    PRIORITY_REALTIME = 4
};

/* Thread state */
enum thread_state {
    THREAD_READY,
    THREAD_RUNNING,
    THREAD_BLOCKED,
    THREAD_SLEEPING,
    THREAD_ZOMBIE
};

/* Thread structure */
struct thread {
    uint32_t tid;
    struct process* process;
    struct cpu_context context;
    enum thread_priority priority;
    enum thread_state state;
    void* stack;
    uintptr_t stack_size;
    uint64_t sleep_until;
    uint64_t time_slice;
    uint64_t cpu_time_ns;  /* CPU time consumed */
    void* user_data;       /* generic user data (e.g. for trampolines) */
    struct thread* next;
    struct thread* prev;
};

/* Global process list (for Lisp introspection) */
extern struct process* process_list;
/* Globals removed for SMP */
/* extern struct process* current_process; */
/* extern struct thread* current_thread; */
extern uint32_t next_pid;
extern uint32_t next_tid;

/* Create process */
struct process* process_create(void);

/* Fork process */
struct process* process_fork(struct process* parent);

/* Destroy process */
void process_destroy(struct process* proc);

/* Create thread */
struct thread* thread_create(struct process* proc, void (*entry)(void));

/* Destroy thread */
void thread_destroy(struct thread* thread);

/* Exit thread */
void thread_exit(void);

/* Get current process */
struct process* process_get_current(void);

/* Get current thread */
struct thread* thread_get_current(void);

/* Execute ELF binary */
int process_exec(const char* path, char** argv, char** envp);

#endif /* PROCESS_H */
