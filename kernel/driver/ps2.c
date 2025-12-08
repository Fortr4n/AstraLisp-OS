/* AstraLisp OS PS/2 Controller Driver - Implementation */

#include "ps2.h"
#include "../input/input.h"
#include "../hal/io.h" /* For inb/outb */
#include <stddef.h>

/* Basic Scancode Set 1 Mapping (US QWERTY) - Simplified for brevity */
/* Index = Scancode */
static uint8_t scan_code_set1[128] = {
    0, 27, '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', '\b',
    '\t', 'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']', '\n',
    0, 'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';', '\'', '`', 0,
    '\\', 'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/', 0, '*',
    0, ' ', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, '-', 0, 0, 0, 
    '+', 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 /* More keys can be mapped */
};

/* Mouse State */
static uint8_t mouse_cycle = 0;
static uint8_t mouse_byte[3];

static inline void ps2_wait_write(void) {
    while ((inb(PS2_STATUS_PORT) & 2) != 0);
}

static inline void ps2_wait_read(void) {
    while ((inb(PS2_STATUS_PORT) & 1) == 0);
}

/* Interrupt Wrappers */
static void ps2_keyboard_wrapper(uint32_t v, void* s) {
    (void)v; (void)s;
    ps2_keyboard_handler();
}

static void ps2_mouse_wrapper(uint32_t v, void* s) {
    (void)v; (void)s;
    ps2_mouse_handler();
}

extern void idt_register_handler(uint32_t vector, void (*handler)(uint32_t, void*));

void ps2_init(void) {
    /* Disable Ports */
    ps2_wait_write();
    outb(PS2_CMD_PORT, PS2_CMD_DISABLE_PORT1);
    ps2_wait_write();
    outb(PS2_CMD_PORT, PS2_CMD_DISABLE_PORT2);
    
    /* Flush Output Buffer */
    inb(PS2_DATA_PORT);
    
    /* Config Byte */
    ps2_wait_write();
    outb(PS2_CMD_PORT, PS2_CMD_READ_CONFIG);
    ps2_wait_read();
    uint8_t config = inb(PS2_DATA_PORT);
    
    /* Enable IRQs (Bit 0=Port1, Bit 1=Port2) & Translation (Bit 6) */
    config |= 0x01; /* Enable Kbd IRQ */
    config |= 0x02; /* Enable Mouse IRQ */
    config |= 0x40; /* Enable Translation (Scan Set 2->1) */
    
    ps2_wait_write();
    outb(PS2_CMD_PORT, PS2_CMD_WRITE_CONFIG);
    ps2_wait_write();
    outb(PS2_DATA_PORT, config);
    
    /* Enable Ports */
    ps2_wait_write();
    outb(PS2_CMD_PORT, PS2_CMD_ENABLE_PORT1);
    ps2_wait_write();
    outb(PS2_CMD_PORT, PS2_CMD_ENABLE_PORT2);
    
    /* Reset Mouse to enable packet streaming */
    /* Write to Port 2 (Mouse) */
    ps2_wait_write();
    outb(PS2_CMD_PORT, 0xD4); /* Write bits to next mouse byte */
    ps2_wait_write();
    outb(PS2_DATA_PORT, 0xF6); /* Set Defaults */
    ps2_wait_read();
    inb(PS2_DATA_PORT); /* Ack */
    
    ps2_wait_write();
    outb(PS2_CMD_PORT, 0xD4);
    ps2_wait_write();
    outb(PS2_DATA_PORT, 0xF4); /* Enable Data Reporting */
    ps2_wait_read();
    inb(PS2_DATA_PORT); /* Ack */

    /* Register Interrupt Handlers */
    /* Map IRQ 1 -> Vector 1, IRQ 12 -> Vector 12 (Simplification) */
    idt_register_handler(1, ps2_keyboard_wrapper);
    idt_register_handler(12, ps2_mouse_wrapper);
}

void ps2_keyboard_handler(void) {
    uint8_t scancode = inb(PS2_DATA_PORT);
    
    /* Check for Break Code (Release) */
    if (scancode & 0x80) {
        /* Released */
        /* Currently we only track presses for character input */
        /* But sophisticated input system tracks separate Up/Down */
        uint8_t key = scan_code_set1[scancode & 0x7F];
        if (key) {
            input_push_event(INPUT_TYPE_KEY, key, KEY_RELEASED);
        }
    } else {
        /* Pressed */
        uint8_t key = scan_code_set1[scancode];
        if (key) {
            input_push_event(INPUT_TYPE_KEY, key, KEY_PRESSED);
        }
    }
}

void ps2_mouse_handler(void) {
    uint8_t byte = inb(PS2_DATA_PORT);
    
    /* PS/2 Mouse sends 3-byte packets (usually) */
    /* Byte 0: Yovfl, Xovfl, Ysign, Xsign, 1, Mid, Right, Left */
    /* Byte 1: X movement */
    /* Byte 2: Y movement */
    
    /* Sync bit check (Byte 0 bit 3 must be 1) helps alignment */
    if (mouse_cycle == 0 && !(byte & 0x08)) {
        return; /* Out of sync */
    }
    
    mouse_byte[mouse_cycle] = byte;
    mouse_cycle++;
    
    if (mouse_cycle == 3) {
        mouse_cycle = 0;
        
        uint8_t flags = mouse_byte[0];
        int32_t x_rel = (int8_t)mouse_byte[1]; /* Cast to signed */
        int32_t y_rel = (int8_t)mouse_byte[2];
        
        /* PS/2 Y is inverted relative to screen (Up is positive in protocol? No, Wait.) */
        /* Standard PS/2: Y increases Up? */
        /* Usually we flip Y for screen coordinates. */
        
        uint32_t buttons = (flags & 0x07);
        
        /* Push separate events or combined? */
        /* Push specific Move/Btn events simplifies consumer */
        
        if (x_rel != 0) input_push_event(INPUT_TYPE_MOUSE, 0, x_rel);
        if (y_rel != 0) input_push_event(INPUT_TYPE_MOUSE, 1, -y_rel); /* Flip Y */
        if (buttons)    input_push_event(INPUT_TYPE_MOUSE, 2, buttons);
    }
}
