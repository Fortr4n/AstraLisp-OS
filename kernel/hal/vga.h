/* AstraLisp OS VGA Text Mode Driver */

#ifndef VGA_H
#define VGA_H

#include <stdint.h>
#include <stddef.h>

/* Initialize VGA text mode */
void vga_init(void);

/* Output character */
void vga_putchar(char c);

/* Output string */
void vga_puts(const char* str);

/* Clear screen */
void vga_clear(void);

/* Set cursor position */
void vga_set_cursor(size_t x, size_t y);

/* Get cursor position */
void vga_get_cursor(size_t* x, size_t* y);

#endif /* VGA_H */
