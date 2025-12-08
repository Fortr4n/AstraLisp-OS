/* AstraLisp OS - Open Power Abstraction Layer (OPAL) Interface */

#ifndef _KERNEL_OPAL_H
#define _KERNEL_OPAL_H

#include <stdint.h>
#include <stddef.h>

/* OPAL Token Definitions */
#define OPAL_CONSOLE_READ           1
#define OPAL_CONSOLE_WRITE          2
#define OPAL_RTC_READ               3
#define OPAL_RTC_WRITE              4
#define OPAL_CEC_POWER_DOWN         5
#define OPAL_CEC_REBOOT             6
#define OPAL_POLL_EVENTS            10
#define OPAL_START_CPU              41
#define OPAL_QUERY_CPU_STATUS       42
#define OPAL_RETURN_CPU             69
#define OPAL_REINIT_CPUS            70

#define OPAL_CONFIG_CPU_IDLE_STATE  115
#define OPAL_SLW_SET_REG            116
#define OPAL_REGISTER_DUMP          117
#define OPAL_PCI_SET_PHB_CAPI_MODE  118
#define OPAL_PCI_GET_PHB_CAPI_MODE_INFO 119

/* XIVE OPAL Tokens */
#define OPAL_XIVE_RESET             128
#define OPAL_XIVE_GET_IRQ_INFO      129
#define OPAL_XIVE_GET_IRQ_CONFIG    130
#define OPAL_XIVE_SET_IRQ_CONFIG    131
#define OPAL_XIVE_GET_QUEUE_INFO    132
#define OPAL_XIVE_SET_QUEUE_INFO    133
#define OPAL_XIVE_DONATE_PAGE       134
#define OPAL_XIVE_ALLOCATE_IRQ      135
#define OPAL_XIVE_FREE_IRQ          136
#define OPAL_XIVE_GET_VP_INFO       137
#define OPAL_XIVE_SET_VP_INFO       138
#define OPAL_XIVE_ALLOCATE_VP_BLOCK 139
#define OPAL_XIVE_FREE_VP_BLOCK     140
#define OPAL_XIVE_ALLOCATE_IRQ_TARGET 141
#define OPAL_XIVE_FREE_IRQ_TARGET   142
#define OPAL_XIVE_GET_PHB_IRQ_SPACE 143
#define OPAL_XIVE_REGISTER_PHB      144
#define OPAL_XIVE_SYNC              145

void opal_putc(char c);
void opal_puts(const char* str);
int opal_getc(void); /* Returns -1 if no input, char cast to int otherwise */

/* Return Codes */
#define OPAL_SUCCESS                0
#define OPAL_PARAMETER              -1
#define OPAL_BUSY                   -2
#define OPAL_PARTIAL                -3
#define OPAL_CONSTRAINED            -4
#define OPAL_CLOSED                 -5
#define OPAL_HARDWARE               -6
#define OPAL_UNSUPPORTED            -7

/* Initialization */
int opal_init(void* fdt);

/* Console */
int64_t opal_console_write(int64_t term_number, uint64_t* length, const uint8_t* buffer);
int64_t opal_console_read(int64_t term_number, uint64_t* length, uint8_t* buffer);
int64_t opal_poll_events(uint64_t* outstanding_event_mask);

/* Power */
int64_t opal_cec_power_down(uint64_t request);
int64_t opal_cec_reboot(void);

/* Entry point type */
typedef int64_t (*opal_entry_func)(int64_t token, ...);

#endif
