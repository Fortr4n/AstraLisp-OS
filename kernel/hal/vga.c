/* AstraLisp OS VGA Text Mode Implementation */

#include "vga.h"

#define VGA_WIDTH 80
#define VGA_HEIGHT 25
#define VGA_MEMORY 0xB8000

/* Color codes */
enum vga_color {
    VGA_COLOR_BLACK = 0,
    VGA_COLOR_BLUE = 1,
    VGA_COLOR_GREEN = 2,
    VGA_COLOR_CYAN = 3,
    VGA_COLOR_RED = 4,
    VGA_COLOR_MAGENTA = 5,
    VGA_COLOR_BROWN = 6,
    VGA_COLOR_LIGHT_GREY = 7,
    VGA_COLOR_DARK_GREY = 8,
    VGA_COLOR_LIGHT_BLUE = 9,
    VGA_COLOR_LIGHT_GREEN = 10,
    VGA_COLOR_LIGHT_CYAN = 11,
    VGA_COLOR_LIGHT_RED = 12,
    VGA_COLOR_LIGHT_MAGENTA = 13,
    VGA_COLOR_LIGHT_BROWN = 14,
    VGA_COLOR_WHITE = 15,
};

static size_t vga_row = 0;
static size_t vga_column = 0;
static uint8_t vga_color = VGA_COLOR_LIGHT_GREY | (VGA_COLOR_BLACK << 4);
static uint16_t* vga_buffer = (uint16_t*)VGA_MEMORY;

/* Create VGA entry */
static inline uint16_t vga_entry(unsigned char uc, uint8_t color) {
    return (uint16_t)uc | (uint16_t)color << 8;
}

/* Initialize VGA */
void vga_init(void) {
    vga_clear();
}

/* Clear screen */
void vga_clear(void) {
    for (size_t y = 0; y < VGA_HEIGHT; y++) {
        for (size_t x = 0; x < VGA_WIDTH; x++) {
            const size_t index = y * VGA_WIDTH + x;
            vga_buffer[index] = vga_entry(' ', vga_color);
        }
    }
    vga_row = 0;
    vga_column = 0;
}

/* Output character */
void vga_putchar(char c) {
    if (c == '\n') {
        vga_column = 0;
        if (++vga_row == VGA_HEIGHT) {
            vga_row = 0;
        }
        return;
    }
    
    if (c == '\r') {
        vga_column = 0;
        return;
    }
    
    if (c == '\t') {
        vga_column = (vga_column + 8) & ~7;
        if (vga_column >= VGA_WIDTH) {
            vga_column = 0;
            if (++vga_row == VGA_HEIGHT) {
                vga_row = 0;
            }
        }
        return;
    }
    
    const size_t index = vga_row * VGA_WIDTH + vga_column;
    vga_buffer[index] = vga_entry(c, vga_color);
    
    if (++vga_column == VGA_WIDTH) {
        vga_column = 0;
        if (++vga_row == VGA_HEIGHT) {
            vga_row = 0;
        }
    }
}

/* Output string */
void vga_puts(const char* str) {
    while (*str) {
        vga_putchar(*str++);
    }
}

/* Set cursor position */
void vga_set_cursor(size_t x, size_t y) {
    if (x < VGA_WIDTH && y < VGA_HEIGHT) {
        vga_column = x;
        vga_row = y;
    }
}

/* Get cursor position */
void vga_get_cursor(size_t* x, size_t* y) {
    if (x) *x = vga_column;
    if (y) *y = vga_row;
}
