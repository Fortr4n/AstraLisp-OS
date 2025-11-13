/* AstraLisp OS Serial Console Implementation */

#include "serial.h"
#include "io.h"

/* UART 16550 registers */
#define COM1_BASE 0x3F8
#define COM2_BASE 0x2F8

#define UART_RBR 0    /* Receiver Buffer Register */
#define UART_THR 0    /* Transmit Holding Register */
#define UART_IER 1    /* Interrupt Enable Register */
#define UART_IIR 2    /* Interrupt Identification Register */
#define UART_FCR 2    /* FIFO Control Register */
#define UART_LCR 3    /* Line Control Register */
#define UART_MCR 4    /* Modem Control Register */
#define UART_LSR 5    /* Line Status Register */
#define UART_MSR 6    /* Modem Status Register */
#define UART_DLL 0    /* Divisor Latch Low */
#define UART_DLH 1    /* Divisor Latch High */

/* Line Status Register bits */
#define UART_LSR_THRE 0x20  /* Transmit Holding Register Empty */
#define UART_LSR_DR   0x01  /* Data Ready */

static uint16_t serial_port = COM1_BASE;

/* Read from UART register */
static inline uint8_t uart_read(uint8_t reg) {
    return inb(serial_port + reg);
}

/* Write to UART register */
static inline void uart_write(uint8_t reg, uint8_t value) {
    outb(serial_port + reg, value);
}

/* Initialize serial port */
void serial_init(void) {
    /* Disable interrupts */
    uart_write(UART_IER, 0x00);
    
    /* Enable DLAB (Divisor Latch Access Bit) */
    uart_write(UART_LCR, 0x80);
    
    /* Set baud rate to 115200 (divisor = 1) */
    uart_write(UART_DLL, 0x01);
    uart_write(UART_DLH, 0x00);
    
    /* 8 bits, no parity, 1 stop bit */
    uart_write(UART_LCR, 0x03);
    
    /* Enable FIFO, clear them, 14-byte threshold */
    uart_write(UART_FCR, 0xC7);
    
    /* Enable interrupts: RTS, DTR */
    uart_write(UART_MCR, 0x0B);
}

/* Output character */
void serial_putchar(char c) {
    /* Wait for transmit buffer to be empty */
    while (!(uart_read(UART_LSR) & UART_LSR_THRE)) {
        /* Busy wait */
    }
    
    /* Send character */
    uart_write(UART_THR, c);
    
    /* Handle newline */
    if (c == '\n') {
        serial_putchar('\r');
    }
}

/* Output string */
void serial_puts(const char* str) {
    while (*str) {
        serial_putchar(*str++);
    }
}

/* Input character (non-blocking) */
int serial_getchar(void) {
    if (uart_read(UART_LSR) & UART_LSR_DR) {
        return uart_read(UART_RBR);
    }
    return -1;
}

/* Check if data available */
int serial_data_available(void) {
    return (uart_read(UART_LSR) & UART_LSR_DR) != 0;
}
