/* AstraLisp OS Kernel Lisp Interface */

#ifndef KERNEL_LISP_H
#define KERNEL_LISP_H

#include <stdint.h>
#include <stddef.h>

/* Forward declarations */
struct lisp_object;

/* Kernel functions callable from Lisp */
struct lisp_object* kernel_get_processes(void);
struct lisp_object* kernel_get_memory_info(void);
struct lisp_object* kernel_get_cpu_info(void);
struct lisp_object* kernel_file_exists(struct lisp_object* path);
struct lisp_object* kernel_read_file(struct lisp_object* path);
struct lisp_object* kernel_write_file(struct lisp_object* path, struct lisp_object* data);
struct lisp_object* kernel_list_directory(struct lisp_object* path);
struct lisp_object* kernel_tcp_connect(struct lisp_object* host, struct lisp_object* port);
struct lisp_object* kernel_tcp_send(struct lisp_object* socket, struct lisp_object* data);
struct lisp_object* kernel_tcp_receive(struct lisp_object* socket);
struct lisp_object* kernel_inspect_process(struct lisp_object* pid);
struct lisp_object* kernel_inspect_thread(struct lisp_object* tid);
struct lisp_object* kernel_inspect_memory(struct lisp_object* addr);
struct lisp_object* kernel_get_stats(void);
struct lisp_object* kernel_hot_patch(struct lisp_object* name, struct lisp_object* new_body);
struct lisp_object* kernel_load_module(struct lisp_object* path);
struct lisp_object* kernel_unload_module(struct lisp_object* name);
struct lisp_object* kernel_profile_start(struct lisp_object* name);
struct lisp_object* kernel_profile_get_results(struct lisp_object* name);
struct lisp_object* kernel_try(struct lisp_object* expr);
struct lisp_object* kernel_spawn_thread(struct lisp_object* func);
struct lisp_object* kernel_thread_join(struct lisp_object* thread);
struct lisp_object* kernel_mutex_create(void);
struct lisp_object* kernel_mutex_lock(struct lisp_object* mutex);
struct lisp_object* kernel_mutex_unlock(struct lisp_object* mutex);
struct lisp_object* kernel_send_message(struct lisp_object* process, struct lisp_object* message);
struct lisp_object* kernel_receive_message(struct lisp_object* timeout);
struct lisp_object* kernel_read_device(struct lisp_object* device, struct lisp_object* offset, struct lisp_object* size);
struct lisp_object* kernel_write_device(struct lisp_object* device, struct lisp_object* offset, struct lisp_object* data);
struct lisp_object* kernel_register_interrupt(struct lisp_object* irq, struct lisp_object* handler);
struct lisp_object* kernel_unregister_interrupt(struct lisp_object* irq);

struct lisp_object* kernel_read_input(void);
struct lisp_object* kernel_poll_input(void);

/* Initialize kernel Lisp interface */
int kernel_lisp_init(void);

/* Register kernel functions in Lisp environment */
int kernel_lisp_register_functions(void);

#endif /* KERNEL_LISP_H */
