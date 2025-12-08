#ifndef LISP_IO_H
#define LISP_IO_H

#ifdef KERNEL
    #include "../../kernel/drivers/opal/opal.h"
    #define LISP_PRINTF(fmt, ...) opal_puts(fmt) /* Warning: No formatting yet */
    #define LISP_PUTS(s) opal_puts(s)
    #define LISP_PUTCHAR(c) opal_putc(c)
#else
    #include <stdio.h>
    #define LISP_PRINTF(fmt, ...) printf(fmt, ##__VA_ARGS__)
    #define LISP_PUTS(s) printf("%s", s)
    #define LISP_PUTCHAR(c) putchar(c)
#endif

#endif
