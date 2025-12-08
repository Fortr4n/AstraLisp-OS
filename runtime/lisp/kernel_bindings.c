/* AstraLisp OS Kernel Bindings for Lisp */
/* Exposes kernel primitives to the Lisp REPL */

#include "evaluator.h"
#include "tagged.h"
#include "types.h"

#ifdef KERNEL
#include "../../kernel/mm/pmm.h"
#include "../../kernel/drivers/opal/opal.h"
#include "../../kernel/process/process.h"
#include "../../kernel/process/scheduler.h"
#include "../../kernel/arch/ppc64/smp.h"
#endif

/* sys:mem-free -> Integer (number of free page frames) */
static lisp_value builtin_mem_free(lisp_value env, lisp_value args) {
    (void)env;
    (void)args;
#ifdef KERNEL
    size_t free_pages = pmm_get_free_count();
    return make_int((int64_t)free_pages);
#else
    return make_int(0);
#endif
}

/* sys:mem-used -> Integer */
static lisp_value builtin_mem_used(lisp_value env, lisp_value args) {
    (void)env;
    (void)args;
#ifdef KERNEL
    size_t used_pages = pmm_get_used_count();
    return make_int((int64_t)used_pages);
#else
    return make_int(0);
#endif
}

/* sys:reboot -> NIL (triggers reboot) */
static lisp_value builtin_reboot(lisp_value env, lisp_value args) {
    (void)env;
    (void)args;
#ifdef KERNEL
    opal_cec_reboot();
    /* Should not return */
#endif
    return LISP_NIL;
}

/* sys:shutdown -> NIL (triggers power off) */
static lisp_value builtin_shutdown(lisp_value env, lisp_value args) {
    (void)env;
    (void)args;
#ifdef KERNEL
    opal_cec_power_down(0);
    /* Should not return */
#endif
    return LISP_NIL;
}

/* sys:cpu-id -> Integer (current CPU PIR) */
static lisp_value builtin_cpu_id(lisp_value env, lisp_value args) {
    (void)env;
    (void)args;
#ifdef KERNEL
    uint64_t pir;
    __asm__ volatile ("mfspr %0, 0x3ff" : "=r"(pir)); /* SPR 1023 = PIR */
    return make_int((int64_t)pir);
#else
    return make_int(0);
#endif
}

/* sys:cpu-count -> Integer (number of online CPUs) */
static lisp_value builtin_cpu_count(lisp_value env, lisp_value args) {
    (void)env;
    (void)args;
#ifdef KERNEL
    return make_int((int64_t)smp_get_cpu_count());
#else
    return make_int(1);
#endif
}

/* sys:boot-cpu id -> T/NIL (boot secondary CPU) */
static lisp_value builtin_boot_cpu(lisp_value env, lisp_value args) {
    (void)env;
#ifdef KERNEL
    if (is_nil(args)) return LISP_NIL;
    lisp_value id_val = car(args);
    if (!is_int(id_val)) return LISP_NIL;
    
    int64_t id = as_int(id_val);
    if (smp_boot_cpu((uint32_t)id) == 0) {
        return lisp_create_symbol("T");
    }
#else
    (void)args;
#endif
    return LISP_NIL;
}

/* sys:heap-size -> Integer (heap stats if implemented) */
static lisp_value builtin_heap_size(lisp_value env, lisp_value args) {
    (void)env;
    (void)args;
    /* Placeholder: Heap stats not implemented yet */
    return make_int(0);
}

/* sys:nvme-read lba blocks -> buffer or NIL */
/* Note: Simplified - real impl would need proper buffer management */
static lisp_value builtin_nvme_read(lisp_value env, lisp_value args) {
    (void)env;
#ifdef KERNEL
    /* TODO: Implement once NVMe controller global is available */
    /* Would require: parse args for LBA, blocks, allocate buffer, call nvme_read */
#else
    (void)args;
#endif
    return LISP_NIL;
}

/* fs:mount device fs-type -> T/NIL */
static lisp_value builtin_fs_mount(lisp_value env, lisp_value args) {
    (void)env;
    (void)args;
    /* TODO: Would call lfsx_mount or vfs_mount */
    return LISP_NIL;
}

/* fs:open path flags -> file-handle or NIL */
static lisp_value builtin_fs_open(lisp_value env, lisp_value args) {
    (void)env;
    (void)args;
    /* TODO: Would call lfsx_open */
    return LISP_NIL;
}

/* Initialize kernel bindings */
void kernel_bindings_init(void) {
    lisp_register_builtin("sys:mem-free", builtin_mem_free);
    lisp_register_builtin("sys:mem-used", builtin_mem_used);
    lisp_register_builtin("sys:reboot", builtin_reboot);
    lisp_register_builtin("sys:shutdown", builtin_shutdown);
    lisp_register_builtin("sys:cpu-id", builtin_cpu_id);
    lisp_register_builtin("sys:cpu-count", builtin_cpu_count);
    lisp_register_builtin("sys:boot-cpu", builtin_boot_cpu);
    lisp_register_builtin("sys:heap-size", builtin_heap_size);
    lisp_register_builtin("sys:nvme-read", builtin_nvme_read);
    lisp_register_builtin("fs:mount", builtin_fs_mount);
    lisp_register_builtin("fs:open", builtin_fs_open);
}
