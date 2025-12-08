/* AstraLisp OS PS/2 Controller Driver - Header */

#ifndef PS2_H
#define PS2_H

#include <stdint.h>

/* Ports */
#define PS2_DATA_PORT    0x60
#define PS2_STATUS_PORT  0x64
#define PS2_CMD_PORT     0x64

/* Commands */
#define PS2_CMD_DISABLE_PORT1 0xAD
#define PS2_CMD_ENABLE_PORT1  0xAE
#define PS2_CMD_DISABLE_PORT2 0xA7
#define PS2_CMD_ENABLE_PORT2  0xA8
#define PS2_CMD_READ_CONFIG   0x20
#define PS2_CMD_WRITE_CONFIG  0x60
#define PS2_CMD_SELF_TEST     0xAA
#define PS2_CMD_PORT1_TEST    0xAB
#define PS2_CMD_PORT2_TEST    0xA9

/* Initialization */
void ps2_init(void);

/* Handlers (called from generic ISR dispatcher in traps.c/interrupts.c) */
void ps2_keyboard_handler(void);
void ps2_mouse_handler(void);

#endif /* PS2_H */
