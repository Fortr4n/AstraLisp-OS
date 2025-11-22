#ifndef LISP_ERROR_H
#define LISP_ERROR_H

#include "tagged.h"
#include <stdint.h>

#ifdef KERNEL
    /* PowerPC 64 setjmp buffer */
    /* r1, r2, r13, r14-r31, lr, cr = 23 registers + alignment padding */
    typedef uint64_t jmp_buf[24];

    /* setjmp/longjmp prototypes */
    int setjmp(jmp_buf env);
    void longjmp(jmp_buf env, int val);
#else
    #include <setjmp.h>
#endif


/* Error Handler Structure */
struct error_handler {
    jmp_buf buf;
    lisp_value callback; /* Optional Lisp callback function */
    struct error_handler* prev;
};

/* Global Error Stack */
extern struct error_handler* error_stack_top;
extern const char* lisp_last_error_msg;

/* Functions */
void lisp_error(const char* msg);

void lisp_push_handler(struct error_handler* handler);
void lisp_pop_handler(void);

#endif
