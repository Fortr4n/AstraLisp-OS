/* AstraLisp OS - SMP Implementation */

#include "smp.h"
#include "../../drivers/opal/opal.h"
#include "../../lib/fdt.h"
#include "../../mm/pmm.h"
#include "../../lib/string.h"

/* Extern for OPAL call */
extern int64_t opal_call(int64_t token, ...);

/* Global Per-CPU Data */
struct per_cpu per_cpu_data[NR_CPUS];
uint32_t nr_cpus_online = 0;
uint32_t nr_cpus_present = 0;

/* Secondary entry address (from assembly) */
extern void secondary_cpu_entry(void);

/* Boot stack size per CPU */
#define SECONDARY_STACK_SIZE (16 * 1024)

/* Parse FDT to enumerate CPUs */
static int enumerate_cpus(void* fdt) {
    /* Find /cpus node */
    int cpus_off = fdt_node_offset_by_path("/cpus");
    if (cpus_off < 0) {
        return -1;
    }
    
    /* Iterate children of /cpus */
    /* Each child is a cpu@X node */
    /* We need fdt_first_subnode / fdt_next_subnode */
    /* For simplicity, assume cpu DT indexes 0, 1, 2, etc */
    /* In real impl, we'd iterate. */
    
    /* For now, just check first CPU (boot CPU is already running) */
    /* and try to find additional cpus via properties */
    
    /* Simple approach: look for ibm,chip-id or reg properties */
    /* Actually, let's just hardcode detection for testing */
    /* or scan memory for known patterns */
    
    /* Production approach: Use fdt iteration */
    /* I'll implement a simple scan for "cpu@" nodes */
    
    /* For POWER, CPUs are listed as /cpus/PowerPC,POWER10@0 etc */
    /* Each has "reg" = PIR */
    
    /* Since we don't have full FDT iteration, use OPAL query */
    /* OPAL_QUERY_CPU_STATUS can be called with PIR */
    
    /* Scan PIRs 0-255 and check status */
    for (uint32_t pir = 0; pir < NR_CPUS; pir++) {
        int64_t status = opal_call(OPAL_QUERY_CPU_STATUS, pir);
        
        /* Status values:
           OPAL_THREAD_INACTIVE = 0
           OPAL_THREAD_STARTED = 1
           OPAL_THREAD_UNAVAILABLE = 2
        */
        
        if (status >= 0 && status != 2) { /* Not unavailable */
            per_cpu_data[nr_cpus_present].cpu_id = nr_cpus_present;
            per_cpu_data[nr_cpus_present].pir = pir;
            per_cpu_data[nr_cpus_present].state = (status == 1) ? CPU_STATE_ONLINE : CPU_STATE_OFFLINE;
            per_cpu_data[nr_cpus_present].stack = NULL;
            per_cpu_data[nr_cpus_present].current_thread = NULL;
            per_cpu_data[nr_cpus_present].ticks = 0;
            
            if (status == 1) {
                nr_cpus_online++;
            }
            
            nr_cpus_present++;
        }
    }
    
    return 0;
}

/* Initialize SMP */
int smp_init(void* fdt) {
    /* Clear per-CPU array */
    memset(per_cpu_data, 0, sizeof(per_cpu_data));
    nr_cpus_online = 1; /* Boot CPU is online */
    nr_cpus_present = 0;
    
    /* Enumerate CPUs from OPAL */
    if (enumerate_cpus(fdt) != 0) {
        /* Fallback: assume single CPU */
        per_cpu_data[0].cpu_id = 0;
        __asm__ volatile ("mfspr %0, 0x3ff" : "=r"(per_cpu_data[0].pir));
        per_cpu_data[0].state = CPU_STATE_ONLINE;
        nr_cpus_present = 1;
        return 0;
    }
    
    return 0;
}

/* Boot a secondary CPU */
int smp_boot_cpu(uint32_t cpu_id) {
    if (cpu_id >= nr_cpus_present) {
        return -1;
    }
    
    struct per_cpu* cpu = &per_cpu_data[cpu_id];
    
    if (cpu->state == CPU_STATE_ONLINE) {
        return 0; /* Already online */
    }
    
    /* Allocate stack */
    void* stack_phys = pmm_alloc_multiple(SECONDARY_STACK_SIZE / 4096);
    if (!stack_phys) {
        return -1;
    }
    
    cpu->stack = (void*)((uintptr_t)stack_phys + SECONDARY_STACK_SIZE);
    cpu->state = CPU_STATE_STARTING;
    
    /* Call OPAL to start CPU */
    /* OPAL_START_CPU(server_no, start_address, r3_value) */
    /* server_no = PIR */
    /* start_address = physical address of entry point */
    /* r3_value = value to pass in r3 (e.g., cpu_id or per_cpu pointer) */
    
    uint64_t entry = (uint64_t)(uintptr_t)secondary_cpu_entry;
    uint64_t r3_val = (uint64_t)(uintptr_t)cpu;
    
    int64_t rc = opal_call(OPAL_START_CPU, cpu->pir, entry, r3_val);
    
    if (rc != OPAL_SUCCESS) {
        cpu->state = CPU_STATE_OFFLINE;
        pmm_free(stack_phys);
        return -1;
    }
    
    /* Wait for CPU to come online (spin with timeout) */
    volatile uint32_t* state_ptr = &cpu->state;
    for (int i = 0; i < 1000000; i++) {
        if (*state_ptr == CPU_STATE_ONLINE) {
            nr_cpus_online++;
            return 0;
        }
        __asm__ volatile ("or 27,27,27"); /* yield */
    }
    
    /* Timeout */
    cpu->state = CPU_STATE_HALTED;
    return -1;
}

/* Get current CPU */
struct per_cpu* smp_get_cpu(void) {
    uint64_t pir;
    __asm__ volatile ("mfspr %0, 0x3ff" : "=r"(pir));
    
    for (uint32_t i = 0; i < nr_cpus_present; i++) {
        if (per_cpu_data[i].pir == (uint32_t)pir) {
            return &per_cpu_data[i];
        }
    }
    
    return NULL;
}

/* Get CPU by ID */
struct per_cpu* smp_get_cpu_by_id(uint32_t cpu_id) {
    if (cpu_id < nr_cpus_present) {
        return &per_cpu_data[cpu_id];
    }
    return NULL;
}

/* Get online count */
uint32_t smp_get_cpu_count(void) {
    return nr_cpus_online;
}

/* Secondary CPU main function (called from assembly) */
void secondary_main(struct per_cpu* cpu) {
    /* Per-CPU initialization */
    cpu->state = CPU_STATE_ONLINE;
    
    /* Enable interrupts if needed */
    /* uint64_t msr;
       __asm__ volatile ("mfmsr %0" : "=r"(msr));
       msr |= (1ULL << 15); // EE
       __asm__ volatile ("mtmsr %0" :: "r"(msr));
    */
    
    /* Enter scheduler idle loop */
    /* In a full implementation, we would call scheduler_run() */
    /* For now, just idle */
    for (;;) {
        __asm__ volatile ("or 27,27,27"); /* yield hint */
    }
}
