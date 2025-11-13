/* AstraLisp OS Bootloader Linker Script for PowerISA */

ENTRY(_start)

SECTIONS
{
    . = 0x100000;  /* Load address */
    
    .multiboot : {
        *(.multiboot_header)
    }
    
    .text : {
        *(.text)
    }
    
    .rodata : {
        *(.rodata)
    }
    
    .data : {
        *(.data)
    }
    
    .bss : {
        __bss_start = .;
        *(.bss)
        __bss_end = .;
    }
    
    /DISCARD/ : {
        *(.comment)
        *(.note*)
    }
}
