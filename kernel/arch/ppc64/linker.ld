/* AstraLisp OS - PowerISA Linker Script */
/* Target: ppc64le */

ENTRY(_start)

SECTIONS
{
    /* Load kernel at 1MB (standard for Multiboot) */
    . = 0x100000;

    kernel_start = .;

    /* Multiboot Header Section */
    .multiboot :
    {
        KEEP(*(.multiboot))
    }

    /* Text Section */
    .text : ALIGN(0x10000)
    {
        *(.text)
        *(.text.*)
    }

    /* Read-Only Data */
    .rodata : ALIGN(0x10000)
    {
        *(.rodata)
        *(.rodata.*)
    }

    /* Read-Write Data */
    .data : ALIGN(0x10000)
    {
        *(.data)
        *(.data.*)
    }

    /* BSS Section (Zero Initialized) */
    .bss : ALIGN(0x10000)
    {
        __bss_start = .;
        *(.bss)
        *(.bss.*)
        *(COMMON)
        __bss_end = .;
    }

    kernel_end = .;

    /* Discard unnecessary sections */
    /DISCARD/ :
    {
        *(.comment)
        *(.eh_frame)
    }
}
