/* AstraLisp OS Hardware Drivers Bindings */

#include "evaluator.h"
#include "objects.h"
#include "tagged.h"
#include "../../kernel/hal/vga.h"
#include "../../kernel/hal/serial.h"
#include <stddef.h>

/* (vga-print string) */
lisp_value builtin_vga_print(lisp_value env, lisp_value args) {
    if (!args || !IS_CONS(args)) return LISP_NIL;
    
    lisp_value str_val = CAR(args);
    if (IS_STRING(str_val)) {
        struct lisp_string* s = (struct lisp_string*)PTR_VAL(str_val);
        vga_puts(s->data);
    }
    return LISP_T;
}

/* (serial-print string) */
lisp_value builtin_serial_print(lisp_value env, lisp_value args) {
    if (!args || !IS_CONS(args)) return LISP_NIL;
    
    lisp_value str_val = CAR(args);
    if (IS_STRING(str_val)) {
        struct lisp_string* s = (struct lisp_string*)PTR_VAL(str_val);
        serial_puts(s->data);
    }
    return LISP_T;
}

/* (serial-read) -> integer (char code) or nil */
lisp_value builtin_serial_read(lisp_value env, lisp_value args) {
    if (serial_data_available()) {
        int c = serial_getchar();
        return MAKE_FIXNUM(c);
    }
    return LISP_NIL;
}

/* (serial-available?) -> t or nil */
lisp_value builtin_serial_available(lisp_value env, lisp_value args) {
    return serial_data_available() ? LISP_T : LISP_NIL;
}

void drivers_init(void) {
    lisp_register_builtin("vga-print", builtin_vga_print);
    lisp_register_builtin("serial-print", builtin_serial_print);
    lisp_register_builtin("serial-read", builtin_serial_read);
    lisp_register_builtin("serial-available?", builtin_serial_available);
}
