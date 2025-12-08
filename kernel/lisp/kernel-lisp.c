/* AstraLisp OS Kernel Lisp Interface Implementation */

#include "kernel-lisp.h"
#include "../../runtime/lisp/reader.h"
#include "../../runtime/lisp/evaluator.h"
#include "../../runtime/runtime.h"
#include "../process/process.h"
#include "../mm/pmm.h"
#include "../mm/vmm.h"
#include "../hal/serial.h"
#include "../mm/heap.h"
#include "../sync/spinlock.h"
#include "../../filesystem/lfsx/lfsx.h"
#include "../driver/storage.h"
#include "../../network/tcpip/tcpip.h"
#include <stddef.h>
#include <string.h>
#include <stdint.h>

static struct lisp_environment* kernel_env = NULL;

/* Helper: convert integer to Lisp object */
static struct lisp_object* int_to_lisp(int64_t value) {
    return lisp_create_integer(value);
}

/* Helper: convert Lisp object to integer */
static int64_t lisp_to_int(struct lisp_object* obj) {
    if (obj && obj->type == LISP_INTEGER) {
        return obj->value.integer;
    }
    return 0;
}

/* Helper: convert Lisp object to string */
static const char* lisp_to_string(struct lisp_object* obj) {
    if (obj && obj->type == LISP_STRING) {
        return obj->value.string.data;
    } else if (obj && obj->type == LISP_SYMBOL) {
        return obj->value.symbol.name;
    }
    return NULL;
}

/* Helper: create list from array */
static struct lisp_object* array_to_list(void** array, size_t count) {
    struct lisp_object* list = lisp_nil();
    for (size_t i = count; i > 0; i--) {
        list = lisp_create_cons((struct lisp_object*)array[i - 1], list);
    }
    return list;
}

/* Get process list */
struct lisp_object* kernel_get_processes(void) {
    extern struct process* process_list;
    struct process* proc = process_list;
    void* process_array[256];
    size_t count = 0;
    
    while (proc && count < 256) {
        struct lisp_object* pid = lisp_create_integer(proc->pid);
        struct lisp_object* pid_pair = lisp_create_cons(lisp_create_symbol("pid"), pid);
        struct lisp_object* proc_obj = lisp_create_cons(pid_pair, lisp_nil());
        process_array[count++] = proc_obj;
        proc = proc->next;
    }
    
    return array_to_list(process_array, count);
}

/* Get memory info */
struct lisp_object* kernel_get_memory_info(void) {
    size_t free = pmm_get_free_count();
    size_t used = pmm_get_used_count();
    
    struct lisp_object* info = lisp_create_object(LISP_CONS);
    struct lisp_object* free_pair = lisp_create_cons(lisp_create_symbol("free"), lisp_create_integer(free));
    struct lisp_object* used_pair = lisp_create_cons(lisp_create_symbol("used"), lisp_create_integer(used));
    
    info = lisp_create_cons(free_pair, lisp_create_cons(used_pair, lisp_nil()));
    return info;
}

/* Get CPU info */
struct lisp_object* kernel_get_cpu_info(void) {
    /* Read Processor Version Register (PVR) */
    uint64_t pvr;
    __asm__ volatile ("mfspr %0, 287" : "=r"(pvr)); /* SPR 287 = PVR */
    
    uint32_t version = (pvr >> 16) & 0xFFFF;
    uint32_t revision = pvr & 0xFFFF;
    
    /* Read Processor ID Register (PIR) */
    uint64_t pir;
    __asm__ volatile ("mfspr %0, 1023" : "=r"(pir)); /* SPR 1023 = PIR */
    
    struct lisp_object* arch = lisp_create_cons(lisp_create_symbol("arch"), lisp_create_string("PowerISA-v3.1", 13));
    struct lisp_object* ver = lisp_create_cons(lisp_create_symbol("version"), lisp_create_integer(version));
    struct lisp_object* rev = lisp_create_cons(lisp_create_symbol("revision"), lisp_create_integer(revision));
    struct lisp_object* cpu = lisp_create_cons(lisp_create_symbol("pir"), lisp_create_integer(pir));
    
    struct lisp_object* info = lisp_create_cons(arch, 
                                lisp_create_cons(ver,
                                lisp_create_cons(rev,
                                lisp_create_cons(cpu, lisp_nil()))));
    return info;
}

/* File exists */
struct lisp_object* kernel_file_exists(struct lisp_object* path) {
    if (!path) {
        return lisp_nil();
    }
    
    const char* path_str = lisp_to_string(path);
    if (!path_str) {
        return lisp_nil();
    }
    
    struct lfsx_file* file = lfsx_open(path_str, 0);
    if (file) {
        lfsx_close(file);
        return lisp_create_integer(1);
    }
    
    return lisp_nil();
}

/* Read file */
struct lisp_object* kernel_read_file(struct lisp_object* path) {
    if (!path) {
        return NULL;
    }
    
    const char* path_str = lisp_to_string(path);
    if (!path_str) {
        return NULL;
    }
    
    struct lfsx_file* file = lfsx_open(path_str, 0);
    if (!file) {
        return NULL;
    }
    
    uint8_t buffer[4096];
    size_t read = lfsx_read(file, buffer, sizeof(buffer));
    lfsx_close(file);
    
    if (read > 0) {
        return lisp_create_string((const char*)buffer, read);
    }
    
    return lisp_nil();
}

/* Write file */
struct lisp_object* kernel_write_file(struct lisp_object* path, struct lisp_object* data) {
    if (!path || !data) {
        return lisp_nil();
    }
    
    const char* path_str = lisp_to_string(path);
    const char* data_str = lisp_to_string(data);
    
    if (!path_str || !data_str) {
        return lisp_nil();
    }
    
    struct lfsx_file* file = lfsx_open(path_str, 1);  /* Write mode */
    if (!file) {
        return lisp_nil();
    }
    
    size_t written = lfsx_write(file, data_str, strlen(data_str));
    lfsx_close(file);
    
    return lisp_create_integer(written);
}

/* List directory */
struct lisp_object* kernel_list_directory(struct lisp_object* path) {
    /* Placeholder - would implement directory listing */
    return lisp_nil();
}

/* TCP connect */
struct lisp_object* kernel_tcp_connect(struct lisp_object* host, struct lisp_object* port) {
    if (!host || !port) {
        return NULL;
    }
    
    /* Placeholder - would implement TCP connection */
    struct socket* sock = socket_create(2, 1, 6);  /* AF_INET, SOCK_STREAM, TCP */
    if (sock) {
        /* Convert to Lisp object */
        return lisp_create_integer(sock->fd);
    }
    
    return lisp_nil();
}

/* TCP send */
struct lisp_object* kernel_tcp_send(struct lisp_object* socket, struct lisp_object* data) {
    if (!socket || !data) {
        return lisp_nil();
    }
    
    uint32_t fd = (uint32_t)lisp_to_int(socket);
    const char* data_str = lisp_to_string(data);
    
    if (!data_str) {
        return lisp_nil();
    }
    
    /* Find socket and send */
    /* Placeholder */
    return lisp_create_integer(strlen(data_str));
}

/* TCP receive */
struct lisp_object* kernel_tcp_receive(struct lisp_object* socket) {
    if (!socket) {
        return NULL;
    }
    
    /* Placeholder */
    return lisp_nil();
}

/* Inspect process */
struct lisp_object* kernel_inspect_process(struct lisp_object* pid) {
    if (!pid) {
        return NULL;
    }
    
    extern struct process* process_list;
    uint32_t pid_val = (uint32_t)lisp_to_int(pid);
    struct process* proc = process_list;
    
    while (proc) {
        if (proc->pid == pid_val) {
            struct lisp_object* pid_pair = lisp_create_cons(lisp_create_symbol("pid"), lisp_create_integer(proc->pid));
            struct lisp_object* info = lisp_create_cons(pid_pair, lisp_nil());
            return info;
        }
        proc = proc->next;
    }
    
    return lisp_nil();
}

/* Inspect thread */
struct lisp_object* kernel_inspect_thread(struct lisp_object* tid) {
    /* Placeholder */
    return lisp_nil();
}

/* Inspect memory */
struct lisp_object* kernel_inspect_memory(struct lisp_object* addr) {
    if (!addr) {
        return NULL;
    }
    
    uintptr_t addr_val = (uintptr_t)lisp_to_int(addr);
    uint8_t value = *((uint8_t*)addr_val);
    
    return lisp_create_integer(value);
}

/* Get kernel stats */
struct lisp_object* kernel_get_stats(void) {
    extern uint32_t nr_cpus_online;
    extern uint32_t nr_cpus_present;
    
    size_t free_mem = pmm_get_free_count() * 4096;
    size_t used_mem = pmm_get_used_count() * 4096;
    
    struct lisp_object* mem_free = lisp_create_cons(lisp_create_symbol("memory-free"), lisp_create_integer(free_mem));
    struct lisp_object* mem_used = lisp_create_cons(lisp_create_symbol("memory-used"), lisp_create_integer(used_mem));
    struct lisp_object* cpus = lisp_create_cons(lisp_create_symbol("cpus-online"), lisp_create_integer(nr_cpus_online));
    struct lisp_object* cpus_p = lisp_create_cons(lisp_create_symbol("cpus-present"), lisp_create_integer(nr_cpus_present));
    
    struct lisp_object* stats = lisp_create_cons(mem_free,
                                 lisp_create_cons(mem_used,
                                 lisp_create_cons(cpus,
                                 lisp_create_cons(cpus_p, lisp_nil()))));
    return stats;
}

/* Hot patch function */
struct lisp_object* kernel_hot_patch(struct lisp_object* name, struct lisp_object* new_body) {
    if (!name || !new_body) {
        return lisp_nil();
    }
    
    /* Placeholder - would implement hot patching */
    return lisp_create_integer(1);
}

/* Load module */
struct lisp_object* kernel_load_module(struct lisp_object* path) {
    if (!path) {
        return lisp_nil();
    }
    
    const char* path_str = lisp_to_string(path);
    if (!path_str) {
        return lisp_nil();
    }
    
    /* Read and evaluate module */
    struct lisp_object* module_code = kernel_read_file(path);
    if (module_code) {
        struct lisp_object* result = runtime_eval(module_code);
        lisp_decref(module_code);
        return result;
    }
    
    return lisp_nil();
}

/* Unload module */
struct lisp_object* kernel_unload_module(struct lisp_object* name) {
    /* Placeholder */
    return lisp_create_integer(1);
}

/* Profile start */
struct lisp_object* kernel_profile_start(struct lisp_object* name) {
    /* Placeholder */
    return lisp_create_integer(1);
}

/* Profile get results */
struct lisp_object* kernel_profile_get_results(struct lisp_object* name) {
    /* Placeholder */
    return lisp_nil();
}

/* Try */
struct lisp_object* kernel_try(struct lisp_object* expr) {
    if (!expr) {
        return NULL;
    }
    
    return runtime_eval(expr);
}

/* Spawn thread */
struct lisp_object* kernel_spawn_thread(struct lisp_object* func) {
    if (!func) {
        return NULL;
    }
    
    extern struct process* current_process;
    if (!current_process) {
        return lisp_nil();
    }
    
    /* Create thread with wrapper that will evaluate the Lisp function */
    /* Note: Real impl needs a trampoline that stores func and calls runtime_eval */
    struct thread* thr = thread_create(current_process, NULL);
    if (thr) {
        return lisp_create_integer(thr->tid);
    }
    
    return lisp_nil();
}

/* Thread join */
struct lisp_object* kernel_thread_join(struct lisp_object* thread) {
    /* Placeholder */
    return lisp_nil();
}

/* Mutex create */
struct lisp_object* kernel_mutex_create(void) {
    spinlock_t* lock = (spinlock_t*)kmalloc(sizeof(spinlock_t));
    if (lock) {
        spinlock_init(lock);
        return lisp_create_integer((int64_t)(uintptr_t)lock);
    }
    return lisp_nil();
}

/* Mutex lock */
struct lisp_object* kernel_mutex_lock(struct lisp_object* mutex) {
    if (!mutex) {
        return lisp_nil();
    }
    
    uintptr_t lock_addr = (uintptr_t)lisp_to_int(mutex);
    spinlock_t* lock = (spinlock_t*)lock_addr;
    
    if (lock) {
        spinlock_acquire(lock);
        return lisp_create_integer(1);
    }
    
    return lisp_nil();
}

/* Mutex unlock */
struct lisp_object* kernel_mutex_unlock(struct lisp_object* mutex) {
    if (!mutex) {
        return lisp_nil();
    }
    
    uintptr_t lock_addr = (uintptr_t)lisp_to_int(mutex);
    spinlock_t* lock = (spinlock_t*)lock_addr;
    
    if (lock) {
        spinlock_release(lock);
        return lisp_create_integer(1);
    }
    
    return lisp_nil();
}

/* Send message */
struct lisp_object* kernel_send_message(struct lisp_object* process, struct lisp_object* message) {
    /* Placeholder */
    return lisp_create_integer(1);
}

/* Receive message */
struct lisp_object* kernel_receive_message(struct lisp_object* timeout) {
    /* Placeholder */
    return lisp_nil();
}

/* Read device */
struct lisp_object* kernel_read_device(struct lisp_object* device, struct lisp_object* offset, struct lisp_object* size) {
    if (!device || !offset || !size) {
        return lisp_nil();
    }
    
    const char* dev_name = lisp_to_string(device);
    uint64_t off = (uint64_t)lisp_to_int(offset);
    size_t sz = (size_t)lisp_to_int(size);
    
    if (!dev_name || sz == 0 || sz > 4096) {
        return lisp_nil();
    }
    
    struct block_device* blk = storage_find_device(dev_name);
    if (!blk) {
        return lisp_nil();
    }
    
    uint8_t* buffer = (uint8_t*)kmalloc(sz);
    if (!buffer) {
        return lisp_nil();
    }
    
    if (storage_read(blk, off, buffer, sz) == 0) {
        struct lisp_object* result = lisp_create_string((const char*)buffer, sz);
        kfree(buffer);
        return result;
    }
    
    kfree(buffer);
    return lisp_nil();
}

/* Write device */
struct lisp_object* kernel_write_device(struct lisp_object* device, struct lisp_object* offset, struct lisp_object* data) {
    if (!device || !offset || !data) {
        return lisp_nil();
    }
    
    const char* dev_name = lisp_to_string(device);
    uint64_t off = (uint64_t)lisp_to_int(offset);
    const char* data_str = lisp_to_string(data);
    
    if (!dev_name || !data_str) {
        return lisp_nil();
    }
    
    size_t sz = strlen(data_str);
    struct block_device* blk = storage_find_device(dev_name);
    if (!blk) {
        return lisp_nil();
    }
    
    if (storage_write(blk, off, data_str, sz) == 0) {
        return lisp_create_integer(sz);
    }
    
    return lisp_nil();
}

/* Register interrupt */
struct lisp_object* kernel_register_interrupt(struct lisp_object* irq, struct lisp_object* handler) {
    if (!irq || !handler) {
        return lisp_nil();
    }
    
    uint32_t irq_val = (uint32_t)lisp_to_int(irq);
    /* Register Lisp handler for interrupt */
    /* Placeholder */
    return lisp_create_integer(1);
}

/* Unregister interrupt */
struct lisp_object* kernel_unregister_interrupt(struct lisp_object* irq) {
    /* Placeholder */
    return lisp_create_integer(1);
}

/* Initialize kernel Lisp interface */
int kernel_lisp_init(void) {
    kernel_env = env_create(NULL);
    if (!kernel_env) {
        return -1;
    }
    
    return 0;
}

/* Register kernel functions in Lisp environment */
int kernel_lisp_register_functions(void) {
    if (!kernel_env) {
        return -1;
    }
    
    /* Register all kernel functions */
    env_define(kernel_env, lisp_create_symbol("kernel-get-processes"), 
               lisp_create_object(LISP_FUNCTION));
    env_define(kernel_env, lisp_create_symbol("kernel-get-memory-info"),
               lisp_create_object(LISP_FUNCTION));
    /* ... register all functions ... */
    
    return 0;
}
