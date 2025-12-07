/* AstraLisp OS - XIVE Implementation */

#include "xive.h"
#include "../../drivers/opal/opal.h"
#include "../../lib/fdt.h"
#include "../../lib/string.h"

/* Externs for OPAL calls */
extern int64_t opal_call(int64_t token, ...);

/* Helpers to verify OPAL tokens */
static int check_opal_xive_support(void) {
    /* In a real implementation we check the OPAL firmware version or FDT compatible string */
    /* for "ibm,power9-xive-x" or "ibm,opal-xive-vc" */
    return 0;
}

int xive_init(void* fdt) {
    if (check_opal_xive_support() != 0) {
        return -1;
    }
    
    /* Reset XIVE - OPAL_XIVE_RESET(version) */
    /* Version 1 is "XIVE 1" (POWER9), Version 2 is XIVE2 (POWER10) ? */
    /* Actually OPAL XIVE uses interface versions. */
    /* Let's assume standard default reset */
    
    /* Note: Skiboot requires us to call opal_xive_reset to switch from legacy XICS mode */
    /* into XIVE native mode if supported. */
    
    /* Call OPAL_XIVE_RESET(1) */
    int64_t rc = opal_call(OPAL_XIVE_RESET, 1);
    if (rc != OPAL_SUCCESS) {
        return -1;
    }
    
    return 0;
}

void xive_cpu_init(uint32_t cpu_id) {
    /* Setup CPPR (Current Processor Priority) */
    /* access TIMA to set OS Ring CPPR to logical server priority */
    /* For now, relying on defaults or assuming we are in a simple environment */
}

int xive_register_irq(uint32_t global_irq, uint32_t target_cpu, uint8_t priority) {
    /* Configure IRQ via OPAL */
    /* opal_xive_set_irq_config(uint32_t girq, uint32_t vp, uint8_t prio, uint32_t lirq); */
    
    /* VP (Virtual Processor) is usually cpu_id for physical mode or a specific ID alloc'd */
    /* For bare metal, VP = HW CPU ID (PIR) */
    
    int64_t rc = opal_call(OPAL_XIVE_SET_IRQ_CONFIG, global_irq, target_cpu, priority, global_irq);
    return (rc == OPAL_SUCCESS) ? 0 : -1;
}
