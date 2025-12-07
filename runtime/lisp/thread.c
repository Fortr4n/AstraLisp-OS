/* AstraLisp OS Threading Implementation
 * 
 * Thread management with stop-the-world GC support.
 */

#include "thread.h"
#include "reader.h"
#include "objects.h"
#include "types.h"
#include "evaluator.h"
#include "../gc/gc.h"
#include <stdio.h>
#include <string.h>

/* Kernel compatibility */
#ifdef KERNEL
#include "../../kernel/mm/heap.h"
#include "../../kernel/process/process.h"
#include "../../kernel/process/scheduler.h"
extern void* kmalloc(size_t size);
extern void kfree(void* ptr);
#else
#include <stdlib.h>
#include <pthread.h>
#define kmalloc malloc
#define kfree free
#endif

/* ========== Global State ========== */

static struct thread_registry registry;
static bool threading_initialized = false;

/* Thread synchronization for stop-the-world */
#ifndef KERNEL
static pthread_mutex_t gc_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_mutex_t registry_mutex = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t gc_cond = PTHREAD_COND_INITIALIZER;
static pthread_key_t current_thread_key;
#endif

static volatile bool gc_stop_requested = false;
static volatile int threads_stopped_count = 0;
static volatile int threads_to_stop = 0;

/* ========== Thread-Local Storage ========== */

#ifndef KERNEL
static void set_current_thread(struct lisp_thread* thread) {
    pthread_setspecific(current_thread_key, thread);
}

static struct lisp_thread* get_current_thread(void) {
    return (struct lisp_thread*)pthread_getspecific(current_thread_key);
}
#else
/* Kernel: would use per-CPU or register-based TLS */
static struct lisp_thread* kernel_current_thread = NULL;
static void set_current_thread(struct lisp_thread* thread) {
    kernel_current_thread = thread;
}
static struct lisp_thread* get_current_thread(void) {
    return kernel_current_thread;
}
#endif

/* ========== Thread Registry Management ========== */

static void registry_add(struct lisp_thread* thread) {
#ifndef KERNEL
    pthread_mutex_lock(&registry_mutex);
#endif
    
    thread->next = registry.threads;
    thread->prev = NULL;
    if (registry.threads) {
        registry.threads->prev = thread;
    }
    registry.threads = thread;
    registry.thread_count++;
    
#ifndef KERNEL
    pthread_mutex_unlock(&registry_mutex);
#endif
}

static void registry_remove(struct lisp_thread* thread) {
#ifndef KERNEL
    pthread_mutex_lock(&registry_mutex);
#endif
    
    if (thread->prev) {
        thread->prev->next = thread->next;
    } else {
        registry.threads = thread->next;
    }
    if (thread->next) {
        thread->next->prev = thread->prev;
    }
    registry.thread_count--;
    
#ifndef KERNEL
    pthread_mutex_unlock(&registry_mutex);
#endif
}

/* ========== Thread Entry Point ========== */

#ifdef KERNEL
/* Kernel thread entry point - called by scheduler with lisp_thread* in r3 */
static void kernel_thread_entry(void) {
    /* Get lisp_thread pointer from register (passed in context) */
    struct lisp_thread* thread;
    __asm__ volatile ("mr %0, 3" : "=r"(thread));
    
    if (!thread) return;
    
    set_current_thread(thread);
    thread->state = THREAD_RUNNING;
    
    /* Initialize VM for this kernel thread */
    if (vm_init(&thread->vm) != 0) {
        thread->state = THREAD_FAILED;
        thread->error = lisp_create_string("VM initialization failed", 25);
        return;
    }
    
    /* Execute the function */
    if (IS_FUNCTION(thread->function) || IS_BUILTIN(thread->function) || IS_VM_CLOSURE(thread->function)) {
        if (IS_FUNCTION(thread->function) || IS_VM_CLOSURE(thread->function)) {
            struct vm_closure* closure = (struct vm_closure*)PTR_VAL(thread->function);
            vm_push(&thread->vm, thread->function);
            vm_result_t result = vm_call(&thread->vm, closure, 0);
            
            if (result == VM_OK) {
                thread->result = vm_get_result(&thread->vm);
                thread->state = THREAD_COMPLETED;
            } else {
                thread->error = lisp_create_string(thread->vm.last_error ? 
                    thread->vm.last_error : "Unknown error", 
                    thread->vm.last_error ? strlen(thread->vm.last_error) : 13);
                thread->state = THREAD_FAILED;
            }
        } else {
            /* Builtin function */
            struct lisp_builtin* builtin = (struct lisp_builtin*)PTR_VAL(thread->function);
            thread->result = builtin->fn(thread->vm.globals, LISP_NIL);
            thread->state = THREAD_COMPLETED;
        }
    } else {
        thread->error = lisp_create_string("Invalid function", 16);
        thread->state = THREAD_FAILED;
    }
    
    /* Cleanup VM */
    vm_free(&thread->vm);
    
    /* Remove from registry if detached */
    if (thread->detached) {
        registry_remove(thread);
        kfree(thread);
    }
    
    /* Thread exit - kernel will clean up */
}
#endif

#ifndef KERNEL
static void* thread_entry(void* arg) {
    struct lisp_thread* thread = (struct lisp_thread*)arg;
    
    set_current_thread(thread);
    thread->state = THREAD_RUNNING;
    
    /* Initialize VM for this thread */
    if (vm_init(&thread->vm) != 0) {
        thread->state = THREAD_FAILED;
        thread->error = lisp_create_string("VM initialization failed", 25);
        return NULL;
    }
    
    /* Execute the function */
    if (IS_FUNCTION(thread->function) || IS_BUILTIN(thread->function)) {
        /* Build empty args list */
        lisp_value args = LISP_NIL;
        
        /* Push function and call */
        vm_push(&thread->vm, thread->function);
        
        if (IS_FUNCTION(thread->function)) {
            struct vm_closure* closure = (struct vm_closure*)PTR_VAL(thread->function);
            vm_result_t result = vm_call(&thread->vm, closure, 0);
            
            if (result == VM_OK) {
                thread->result = vm_get_result(&thread->vm);
                thread->state = THREAD_COMPLETED;
            } else {
                thread->error = lisp_create_string(thread->vm.last_error ? 
                    thread->vm.last_error : "Unknown error", 
                    thread->vm.last_error ? strlen(thread->vm.last_error) : 13);
                thread->state = THREAD_FAILED;
            }
        } else {
            /* Builtin function */
            struct lisp_builtin* builtin = (struct lisp_builtin*)PTR_VAL(thread->function);
            thread->result = builtin->fn(thread->vm.globals, LISP_NIL);
            thread->state = THREAD_COMPLETED;
        }
    } else {
        thread->error = lisp_create_string("Invalid function", 16);
        thread->state = THREAD_FAILED;
    }
    
    /* Cleanup VM */
    vm_free(&thread->vm);
    
    /* If detached, cleanup resources */
    if (thread->detached) {
        registry_remove(thread);
        kfree(thread);
    }
    
    return NULL;
}
#endif

/* ========== Public API ========== */

int thread_init(void) {
    if (threading_initialized) return 0;
    
    memset(&registry, 0, sizeof(registry));
    registry.next_id = 1;
    
#ifndef KERNEL
    if (pthread_key_create(&current_thread_key, NULL) != 0) {
        return -1;
    }
#endif
    
    /* Create main thread structure */
    registry.main_thread = (struct lisp_thread*)kmalloc(sizeof(struct lisp_thread));
    if (!registry.main_thread) return -1;
    
    memset(registry.main_thread, 0, sizeof(struct lisp_thread));
    registry.main_thread->id = registry.next_id++;
    strncpy(registry.main_thread->name, "main", sizeof(registry.main_thread->name));
    registry.main_thread->state = THREAD_RUNNING;
    
    registry_add(registry.main_thread);
    set_current_thread(registry.main_thread);
    
    threading_initialized = true;
    return 0;
}

void thread_shutdown(void) {
    if (!threading_initialized) return;
    
    /* Wait for all non-main threads to complete */
    struct lisp_thread* thread = registry.threads;
    while (thread) {
        struct lisp_thread* next = thread->next;
        if (thread != registry.main_thread) {
            if (thread->state == THREAD_RUNNING) {
                thread_join(thread);
            }
            kfree(thread);
        }
        thread = next;
    }
    
    kfree(registry.main_thread);
    
#ifndef KERNEL
    pthread_key_delete(current_thread_key);
#endif
    
    threading_initialized = false;
}

struct lisp_thread* thread_current(void) {
    return get_current_thread();
}

struct lisp_thread* thread_create(lisp_value function, const char* name) {
    struct lisp_thread* thread = (struct lisp_thread*)kmalloc(sizeof(struct lisp_thread));
    if (!thread) return NULL;
    
    memset(thread, 0, sizeof(struct lisp_thread));
    thread->id = registry.next_id++;
    if (name) {
        strncpy(thread->name, name, sizeof(thread->name) - 1);
    } else {
        snprintf(thread->name, sizeof(thread->name), "thread-%llu", 
                 (unsigned long long)thread->id);
    }
    thread->function = function;
    thread->state = THREAD_CREATED;
    thread->result = LISP_NIL;
    thread->error = LISP_NIL;
    
    registry_add(thread);
    
    return thread;
}

int thread_start(struct lisp_thread* thread) {
    if (!thread || thread->state != THREAD_CREATED) {
        return -1;
    }
    
#ifndef KERNEL
    pthread_t handle;
    if (pthread_create(&handle, NULL, thread_entry, thread) != 0) {
        return -1;
    }
    thread->native_handle = (void*)handle;
#else
    /* Kernel mode: Use kernel thread API */
    extern struct process* current_process;
    
    /* Create kernel thread in current process */
    struct thread* kthread = thread_create(current_process, (void (*)(void))kernel_thread_entry);
    if (!kthread) {
        return -1;
    }
    
    /* Store lisp_thread pointer in kernel thread's context for entry function */
    /* The kernel thread entry will retrieve this and call into Lisp VM */
    kthread->context.gpr[3] = (uint64_t)thread;  /* Pass as first arg in r3 */
    
    /* Add to scheduler */
    if (scheduler_add_thread(kthread) != 0) {
        thread_destroy(kthread);
        return -1;
    }
    
    thread->kernel_thread = kthread;
    thread->state = THREAD_RUNNING;
#endif
    
    return 0;
}

lisp_value thread_join(struct lisp_thread* thread) {
    if (!thread) return LISP_NIL;
    
    if (thread == thread_current()) {
        /* Cannot join self */
        return LISP_NIL;
    }
    
#ifndef KERNEL
    if (thread->native_handle) {
        pthread_join((pthread_t)thread->native_handle, NULL);
    }
#endif
    
    lisp_value result = thread->result;
    
    /* Cleanup if not detached */
    if (!thread->detached) {
        registry_remove(thread);
        kfree(thread);
    }
    
    return result;
}

void thread_detach(struct lisp_thread* thread) {
    if (!thread) return;
    thread->detached = true;
    
#ifndef KERNEL
    if (thread->native_handle) {
        pthread_detach((pthread_t)thread->native_handle);
    }
#endif
}

void thread_yield(void) {
#ifndef KERNEL
    pthread_yield();
#endif
}

/* ========== GC Safe Points ========== */

void thread_enter_safe_point(void) {
    struct lisp_thread* thread = thread_current();
    if (!thread) return;
    
    thread->gc_safe_point = true;
    
    /* If GC is waiting for us */
#ifndef KERNEL
    pthread_mutex_lock(&gc_mutex);
    if (gc_stop_requested) {
        threads_stopped_count++;
        pthread_cond_signal(&gc_cond);
        
        /* Wait until GC is done */
        while (gc_stop_requested) {
            pthread_cond_wait(&gc_cond, &gc_mutex);
        }
        threads_stopped_count--;
    }
    pthread_mutex_unlock(&gc_mutex);
#endif
    
    thread->gc_safe_point = false;
}

void thread_leave_safe_point(void) {
    struct lisp_thread* thread = thread_current();
    if (thread) {
        thread->gc_safe_point = false;
    }
}

/* ========== Stop-the-World GC ========== */

void threads_request_stop(void) {
#ifndef KERNEL
    pthread_mutex_lock(&gc_mutex);
#endif
    
    gc_stop_requested = true;
    threads_stopped_count = 0;
    threads_to_stop = 0;
    
    /* Count running threads (excluding current) */
    struct lisp_thread* thread = registry.threads;
    while (thread) {
        if (thread != thread_current() && thread->state == THREAD_RUNNING) {
            threads_to_stop++;
        }
        thread = thread->next;
    }
    
#ifndef KERNEL
    pthread_mutex_unlock(&gc_mutex);
#endif
}

void threads_wait_stopped(void) {
#ifndef KERNEL
    pthread_mutex_lock(&gc_mutex);
    while (threads_stopped_count < threads_to_stop) {
        pthread_cond_wait(&gc_cond, &gc_mutex);
    }
    pthread_mutex_unlock(&gc_mutex);
#endif
}

void threads_resume(void) {
#ifndef KERNEL
    pthread_mutex_lock(&gc_mutex);
    gc_stop_requested = false;
    pthread_cond_broadcast(&gc_cond);
    pthread_mutex_unlock(&gc_mutex);
#endif
}

void threads_mark_roots(void) {
    struct lisp_thread* thread = registry.threads;
    while (thread) {
        /* Mark function and result */
        if (thread->function) gc_mark(thread->function);
        if (thread->result) gc_mark(thread->result);
        if (thread->error) gc_mark(thread->error);
        
        /* Mark thread's VM stack */
        for (lisp_value* slot = thread->vm.stack; slot < thread->vm.stack_top; slot++) {
            if (*slot) gc_mark(*slot);
        }
        
        /* Mark globals */
        if (thread->vm.globals) gc_mark(thread->vm.globals);
        
        thread = thread->next;
    }
}

/* ========== Built-in Functions ========== */

/* Create a Lisp thread handle object from internal thread pointer */
static lisp_value make_thread_handle(struct lisp_thread* thread) {
    struct lisp_thread_handle* handle = (struct lisp_thread_handle*)gc_alloc(
        sizeof(struct lisp_thread_handle));
    if (!handle) return LISP_NIL;
    
    SET_TYPE(&handle->header, TYPE_THREAD);
    handle->thread_ptr = thread;
    handle->thread_id = thread->id;
    
    return PTR_TO_VAL(handle);
}

/* Extract internal thread pointer from Lisp thread handle */
static struct lisp_thread* get_thread_from_handle(lisp_value val) {
    if (!IS_THREAD(val)) return NULL;
    struct lisp_thread_handle* handle = (struct lisp_thread_handle*)PTR_VAL(val);
    return (struct lisp_thread*)handle->thread_ptr;
}

lisp_value builtin_spawn(lisp_value env, lisp_value args) {
    if (!IS_CONS(args)) {
        return LISP_NIL;
    }
    
    lisp_value fn = CAR(args);
    if (!IS_FUNCTION(fn) && !IS_BUILTIN(fn) && !IS_VM_CLOSURE(fn)) {
        return LISP_NIL;
    }
    
    struct lisp_thread* thread = thread_create(fn, NULL);
    if (!thread) {
        return LISP_NIL;
    }
    
    if (thread_start(thread) != 0) {
        registry_remove(thread);
        kfree(thread);
        return LISP_NIL;
    }
    
    /* Return proper thread handle object */
    return make_thread_handle(thread);
}

lisp_value builtin_join(lisp_value env, lisp_value args) {
    if (!IS_CONS(args)) {
        return LISP_NIL;
    }
    
    lisp_value thread_val = CAR(args);
    struct lisp_thread* thread = get_thread_from_handle(thread_val);
    if (!thread) {
        return LISP_NIL;
    }
    
    return thread_join(thread);
}

lisp_value builtin_thread_yield(lisp_value env, lisp_value args) {
    thread_yield();
    return LISP_NIL;
}

lisp_value builtin_current_thread(lisp_value env, lisp_value args) {
    struct lisp_thread* thread = thread_current();
    if (!thread) return LISP_NIL;
    return make_thread_handle(thread);
}

lisp_value builtin_threadp(lisp_value env, lisp_value args) {
    if (!IS_CONS(args)) return LISP_NIL;
    return IS_THREAD(CAR(args)) ? LISP_T : LISP_NIL;
}

lisp_value builtin_thread_state(lisp_value env, lisp_value args) {
    if (!IS_CONS(args)) {
        return LISP_NIL;
    }
    
    struct lisp_thread* thread = get_thread_from_handle(CAR(args));
    if (!thread) {
        return LISP_NIL;
    }
    
    switch (thread->state) {
        case THREAD_CREATED: return lisp_create_symbol("created");
        case THREAD_RUNNING: return lisp_create_symbol("running");
        case THREAD_BLOCKED: return lisp_create_symbol("blocked");
        case THREAD_COMPLETED: return lisp_create_symbol("completed");
        case THREAD_FAILED: return lisp_create_symbol("failed");
        default: return LISP_NIL;
    }
}
