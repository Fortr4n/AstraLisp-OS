/* AstraLisp OS Serial Console (UART 16550) */

#ifndef SERIAL_H
#define SERIAL_H

#include <stdint.h>

/* Initialize serial port */
void serial_init(void);

/* Output character */
void serial_putchar(char c);

/* Output string */
void serial_puts(const char* str);

/* Input character (non-blocking) */
int serial_getchar(void);

/* Check if data available */
int serial_data_available(void);

#endif /* SERIAL_H */
