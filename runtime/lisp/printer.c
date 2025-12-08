/* AstraLisp OS Lisp Printer */
/* Implements lisp_print from reader.h */

#include "reader.h"
#include "types.h"
#include "lisp_io.h"
#include "tagged.h"
#include <stdint.h>
#include <stddef.h>

static void print_rec(lisp_value obj, bool start_list);

static void print_int(int64_t n) {
    char buffer[32];
    int i = 0;
    int sign = 0;
    
    if (n == 0) {
        LISP_PUTS("0");
        return;
    }
    
    if (n < 0) {
        sign = 1;
        n = -n;
    }
    
    while (n > 0) {
        buffer[i++] = (n % 10) + '0';
        n /= 10;
    }
    
    if (sign) LISP_PUTCHAR('-');
    
    while (i > 0) {
        LISP_PUTCHAR(buffer[--i]);
    }
}

static void print_string(lisp_value obj) {
    if (!is_type(obj, TYPE_STRING)) return;
    struct lisp_string* str = (struct lisp_string*)untag(obj);
    LISP_PUTCHAR('"');
    for (size_t i = 0; i < str->length; i++) {
        char c = str->data[i];
        if (c == '"' || c == '\\') {
            LISP_PUTCHAR('\\');
        }
        LISP_PUTCHAR(c);
    }
    LISP_PUTCHAR('"');
}

static void print_symbol(lisp_value obj) {
    if (!is_type(obj, TYPE_SYMBOL)) return;
    struct lisp_symbol* sym = (struct lisp_symbol*)untag(obj);
    LISP_PUTS(sym->name);
}

void lisp_print(lisp_value obj) {
    print_rec(obj, true);
}

static void print_rec(lisp_value obj, bool start_list) {
    if (is_nil(obj)) {
        LISP_PUTS("NIL");
        return;
    }
    
    if (is_int(obj)) {
        print_int(as_int(obj));
        return;
    }
    
    switch (get_type(obj)) {
        case TYPE_CONS: {
            if (start_list) LISP_PUTCHAR('(');
            
            lisp_value car_val = car(obj);
            lisp_value cdr_val = cdr(obj);
            
            print_rec(car_val, true);
            
            if (is_nil(cdr_val)) {
                /* End of list */
            } else if (is_type(cdr_val, TYPE_CONS)) {
                LISP_PUTCHAR(' ');
                print_rec(cdr_val, false);
            } else {
                /* Dotted pair */
                LISP_PUTS(" . ");
                print_rec(cdr_val, true);
            }
            
            if (start_list) LISP_PUTCHAR(')');
            break;
        }
        case TYPE_SYMBOL:
            print_symbol(obj);
            break;
        case TYPE_STRING:
            print_string(obj);
            break;
        case TYPE_FUNCTION:
            LISP_PUTS("<#FUNCTION>");
            break;
        case TYPE_BUILTIN:
            LISP_PUTS("<#BUILTIN>");
            break;
        case TYPE_ENV:
            LISP_PUTS("<#ENV>");
            break;
        case TYPE_VECTOR:
             LISP_PUTS("#(");
             /* TODO: Print vector elements */
             LISP_PUTS(")");
             break;
        default:
            LISP_PUTS("<?>");
            break;
    }
}
