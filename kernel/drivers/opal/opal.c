/* AstraLisp OS - OPAL Driver Implementation */

#include "opal.h"
#include "../../lib/fdt.h"
#include "../../lib/string.h"

/* Externs from asm */
extern uint64_t opal_entry_point;
extern uint64_t opal_base;
extern int64_t opal_call(int64_t token, ...);

int opal_init(void* fdt) {
    if (fdt_init(fdt) != 0) {
        return -1;
    }
    
    /* Find /ibm,opal node */
    int off = fdt_node_offset_by_path("/ibm,opal");
    if (off < 0) {
        return -1;
    }
    
    /* Get entry */
    uint64_t* entry_prop;
    int len;
    if (fdt_get_prop(off, "opal-entry-address", (void**)&entry_prop, &len) != 0) {
        return -1;
    }
    opal_entry_point = fdt64_to_cpu(*entry_prop);
    
    /* Get base */
    uint64_t* base_prop;
    if (fdt_get_prop(off, "opal-base-address", (void**)&base_prop, &len) != 0) {
        return -1;
    }
    opal_base = fdt64_to_cpu(*base_prop);
    
    return 0;
}

int64_t opal_console_write(int64_t term_number, uint64_t* length, const uint8_t* buffer) {
    return opal_call(OPAL_CONSOLE_WRITE, term_number, length, buffer);
}

int64_t opal_console_read(int64_t term_number, uint64_t* length, uint8_t* buffer) {
    return opal_call(OPAL_CONSOLE_READ, term_number, length, buffer);
}

int64_t opal_cec_power_down(uint64_t request) {
    return opal_call(OPAL_CEC_POWER_DOWN, request);
}

/* Higher level console abstraction */
void opal_putc(char c) {
    uint64_t len = 1;
    uint8_t buf = (uint8_t)c;
    opal_console_write(0, &len, &buf); /* Term 0 */
}

void opal_puts(const char* str) {
    uint64_t len = strlen(str);
    opal_console_write(0, &len, (const uint8_t*)str);
}
