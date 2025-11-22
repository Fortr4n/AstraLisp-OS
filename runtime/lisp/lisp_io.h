#ifndef LISP_IO_H
#define LISP_IO_H

#ifdef KERNEL
    #include "../../kernel/hal/serial.h"
    #define LISP_PRINTF(fmt, ...) /* No printf in kernel yet, use serial_puts for simple strings */
    #define LISP_PUTS(s) serial_puts(s)
    #define LISP_PUTCHAR(c) serial_putchar(c)
#else
    #include <stdio.h>
    #define LISP_PRINTF(fmt, ...) printf(fmt, ##__VA_ARGS__)
    #define LISP_PUTS(s) printf("%s", s)
    #define LISP_PUTCHAR(c) putchar(c)
#endif

#endif
