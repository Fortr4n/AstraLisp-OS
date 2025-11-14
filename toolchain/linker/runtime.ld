/* AstraLisp OS Runtime Linker Script for PowerISA */
/* Defines memory layout for Lisp runtime */

ENTRY(runtime_init)

MEMORY
{
    /* Runtime code and data */
    runtime_code : ORIGIN = 0x10000000, LENGTH = 32M
    /* Runtime heap for Lisp objects */
    runtime_heap : ORIGIN = 0x12000000, LENGTH = 256M
    /* Stack for runtime */
    runtime_stack : ORIGIN = 0x22000000, LENGTH = 16M
}

SECTIONS
{
    /* Runtime code */
    .text : {
        _runtime_text_start = .;
        *(.text)
        *(.text.*)
        . = ALIGN(4096);
        _runtime_text_end = .;
    } > runtime_code

    /* Read-only data */
    .rodata : {
        _runtime_rodata_start = .;
        *(.rodata)
        *(.rodata.*)
        . = ALIGN(4096);
        _runtime_rodata_end = .;
    } > runtime_code

    /* Read-write data */
    .data : {
        _runtime_data_start = .;
        *(.data)
        *(.data.*)
        . = ALIGN(4096);
        _runtime_data_end = .;
    } > runtime_code

    /* Uninitialized data */
    .bss : {
        _runtime_bss_start = .;
        *(.bss)
        *(.bss.*)
        *(COMMON)
        . = ALIGN(4096);
        _runtime_bss_end = .;
    } > runtime_code

    /* Lisp heap */
    .lisp_heap : {
        _lisp_heap_start = .;
        . = . + 256M;
        _lisp_heap_end = .;
    } > runtime_heap

    /* Runtime stack */
    .stack : {
        _runtime_stack_bottom = .;
        . = . + 16M;
        _runtime_stack_top = .;
    } > runtime_stack

    /* Symbols */
    _runtime_start = _runtime_text_start;
    _runtime_end = _runtime_bss_end;
    _runtime_size = _runtime_end - _runtime_start;
}

