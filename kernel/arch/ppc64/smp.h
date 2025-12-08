/* AstraLisp OS - SMP (Symmetric Multi-Processing) Support */
/* Target: PowerISA v3.1 (POWER10) */

#ifndef _KERNEL_ARCH_PPC64_SMP_H
#define _KERNEL_ARCH_PPC64_SMP_H

#include <stdint.h>
#include <stdbool.h>

/* Maximum CPUs supported */
#define NR_CPUS 256

/* CPU States */
#define CPU_STATE_OFFLINE   0
#define CPU_STATE_STARTING  1
#define CPU_STATE_ONLINE    2
#define CPU_STATE_HALTED    3

/* Per-CPU Data Structure */
struct per_cpu {
    uint32_t cpu_id;        /* Logical CPU ID */
    uint32_t pir;           /* Hardware PIR */
    uint32_t state;         /* CPU_STATE_xxx */
    void*    stack;         /* Boot stack pointer */
    void*    current_thread;/* Currently running thread */
    uint64_t ticks;         /* Per-CPU tick counter */
};

/* Global Per-CPU Array */
extern struct per_cpu per_cpu_data[NR_CPUS];
extern uint32_t nr_cpus_online;
extern uint32_t nr_cpus_present;

/* Initialize SMP subsystem */
int smp_init(void* fdt);

/* Boot a specific CPU */
int smp_boot_cpu(uint32_t cpu_id);

/* Get number of online CPUs */
uint32_t smp_get_cpu_count(void);

/* Get per-CPU data for current CPU */
struct per_cpu* smp_get_cpu(void);

/* Get per-CPU data by ID */
struct per_cpu* smp_get_cpu_by_id(uint32_t cpu_id);

/* Secondary CPU entry point (defined in assembly) */
void secondary_cpu_entry(void);

#endif
