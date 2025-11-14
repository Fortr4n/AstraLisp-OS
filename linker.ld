/* AstraLisp OS Kernel Linker Script for PowerISA */

ENTRY(_start)

PHDRS
{
    text PT_LOAD FLAGS(5);  /* Read + Execute */
    data PT_LOAD FLAGS(6);  /* Read + Write */
    bss PT_LOAD FLAGS(6);   /* Read + Write */
}

SECTIONS
{
    . = 0x1000000;  /* Kernel load address */
    
    .text : {
        *(.text)
        *(.text.*)
    } :text
    
    .rodata : {
        *(.rodata)
        *(.rodata.*)
    } :text
    
    .data : {
        *(.data)
        *(.data.*)
    } :data
    
    .bss : {
        __bss_start = .;
        *(.bss)
        *(.bss.*)
        *(COMMON)
        __bss_end = .;
    } :bss
    
    /DISCARD/ : {
        *(.comment)
        *(.note*)
        *(.eh_frame)
    }
}
