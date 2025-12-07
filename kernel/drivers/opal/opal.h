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
