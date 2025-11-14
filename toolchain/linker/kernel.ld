/* AstraLisp OS Kernel Linker Script for PowerISA */
/* Defines memory layout and sections for the kernel */

ENTRY(_start)

/* Memory layout for PowerISA pseries (QEMU) */
MEMORY
{
    /* Kernel code and data at 0x1000000 (16MB) */
    kernel_start : ORIGIN = 0x01000000, LENGTH = 16M
    /* Kernel heap */
    kernel_heap  : ORIGIN = 0x02000000, LENGTH = 64M
    /* Stack grows downward from 0x08000000 */
    stack        : ORIGIN = 0x08000000, LENGTH = 8M
}

SECTIONS
{
    /* Bootloader entry point */
    .boot : {
        *(.boot)
        *(.multiboot)
    } > kernel_start

    /* Kernel code */
    .text : {
        _text_start = .;
        *(.text)
        *(.text.*)
        . = ALIGN(4096);
        _text_end = .;
    } > kernel_start

    /* Read-only data */
    .rodata : {
        _rodata_start = .;
        *(.rodata)
        *(.rodata.*)
        . = ALIGN(4096);
        _rodata_end = .;
    } > kernel_start

    /* Read-write data */
    .data : {
        _data_start = .;
        *(.data)
        *(.data.*)
        . = ALIGN(4096);
        _data_end = .;
    } > kernel_start

    /* Uninitialized data */
    .bss : {
        _bss_start = .;
        *(.bss)
        *(.bss.*)
        *(COMMON)
        . = ALIGN(4096);
        _bss_end = .;
    } > kernel_start

    /* Kernel heap */
    .heap : {
        _heap_start = .;
        . = . + 64M;
        _heap_end = .;
    } > kernel_heap

    /* Stack */
    .stack : {
        _stack_bottom = .;
        . = . + 8M;
        _stack_top = .;
    } > stack

    /* Symbols for kernel */
    _kernel_start = _text_start;
    _kernel_end = _bss_end;
    _kernel_size = _kernel_end - _kernel_start;
}

