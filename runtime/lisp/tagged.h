/* AstraLisp OS Tagged Pointer System
 * 
 * 64-bit Tagged Pointer Scheme (Low-tagging):
 * 
 * Bits [2:0] determine the type:
 * 000 (0): Heap Object Pointer (8-byte aligned)
 * 001 (1): Fixnum (63-bit signed integer)
 * 010 (2): Character (32-bit Unicode)
 * 011 (3): Immediate Constant (NIL, T, Unbound, etc.)
 * 
 * Heap Objects have a header word that defines their specific type.
 */

#ifndef TAGGED_H
#define TAGGED_H

#include <stdint.h>
#include <stdbool.h>

typedef uintptr_t lisp_value;

/* Tag Masks and Values */
#define TAG_MASK        0x7ULL
#define TAG_POINTER     0x0ULL
#define TAG_FIXNUM      0x1ULL
#define TAG_CHAR        0x2ULL
#define TAG_IMMEDIATE   0x3ULL

/* Immediate Constants */
#define LISP_NIL        (0x00ULL | TAG_IMMEDIATE) /* NIL is 0 | TAG */
#define LISP_T          (0x10ULL | TAG_IMMEDIATE)
#define LISP_UNBOUND    (0x20ULL | TAG_IMMEDIATE)
#define LISP_EOF        (0x30ULL | TAG_IMMEDIATE)

/* Type Checking Macros */
#define IS_POINTER(x)   (((x) & TAG_MASK) == TAG_POINTER)
#define IS_FIXNUM(x)    (((x) & TAG_MASK) == TAG_FIXNUM)
#define IS_CHAR(x)      (((x) & TAG_MASK) == TAG_CHAR)
#define IS_IMMEDIATE(x) (((x) & TAG_MASK) == TAG_IMMEDIATE)

#define IS_NIL(x)       ((x) == LISP_NIL)
#define IS_T(x)         ((x) == LISP_T)

/* Fixnum Operations */
/* Shift right by 3 to get raw integer, preserving sign */
#define FIXNUM_VAL(x)   ((int64_t)(x) >> 3)
/* Shift left by 3 and add tag */
#define MAKE_FIXNUM(x)  (((uintptr_t)(x) << 3) | TAG_FIXNUM)

/* Character Operations */
#define CHAR_VAL(x)     ((uint32_t)((x) >> 8))
#define MAKE_CHAR(x)    (((uintptr_t)(x) << 8) | TAG_CHAR)

/* Pointer Operations */
/* Mask out tag bits (though they should be 0 for pointers) */
#define PTR_VAL(x)      ((void*)((x) & ~TAG_MASK))
#define MAKE_PTR(x)     ((lisp_value)(x))
#define PTR_TO_VAL(x)   MAKE_PTR(x)

#endif /* TAGGED_H */
