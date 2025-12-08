/* AstraLisp OS - ELF64 Format */

#ifndef ELF_H
#define ELF_H

#include <stdint.h>

/* Types */
typedef uint64_t Elf64_Addr;
typedef uint64_t Elf64_Off;
typedef uint16_t Elf64_Half;
typedef uint32_t Elf64_Word;
typedef int32_t  Elf64_Sword;
typedef uint64_t Elf64_Xword;
typedef int64_t  Elf64_Sxword;

/* ELF Header */
#define EI_NIDENT 16

typedef struct {
    unsigned char e_ident[EI_NIDENT];
    Elf64_Half    e_type;
    Elf64_Half    e_machine;
    Elf64_Word    e_version;
    Elf64_Addr    e_entry;    /* Entry Point */
    Elf64_Off     e_phoff;    /* Program Header Offset */
    Elf64_Off     e_shoff;    /* Section Header Offset */
    Elf64_Word    e_flags;
    Elf64_Half    e_ehsize;
    Elf64_Half    e_phentsize;/* PH Entry Size */
    Elf64_Half    e_phnum;    /* PH Count */
    Elf64_Half    e_shentsize;
    Elf64_Half    e_shnum;
    Elf64_Half    e_shstrndx;
} Elf64_Ehdr;

/* e_ident indices */
#define EI_MAG0    0
#define EI_MAG1    1
#define EI_MAG2    2
#define EI_MAG3    3
#define EI_CLASS   4
#define EI_DATA    5
#define EI_VERSION 6
#define EI_OSABI   7
#define EI_ABIVERSION 8

/* Magic */
#define ELFMAG0    0x7f
#define ELFMAG1    'E'
#define ELFMAG2    'L'
#define ELFMAG3    'F'

/* Program Header */
typedef struct {
    Elf64_Word    p_type;     /* Segment type */
    Elf64_Word    p_flags;    /* Segment flags */
    Elf64_Off     p_offset;   /* Segment file offset */
    Elf64_Addr    p_vaddr;    /* Segment virtual address */
    Elf64_Addr    p_paddr;    /* Segment physical address */
    Elf64_Xword   p_filesz;   /* Segment size in file */
    Elf64_Xword   p_memsz;    /* Segment size in memory */
    Elf64_Xword   p_align;    /* Segment alignment */
} Elf64_Phdr;

/* p_type */
#define PT_NULL    0
#define PT_LOAD    1
#define PT_DYNAMIC 2
#define PT_INTERP  3
#define PT_NOTE    4
#define PT_SHLIB   5
#define PT_PHDR    6

/* p_flags */
#define PF_X       (1 << 0)
#define PF_W       (1 << 1)
#define PF_R       (1 << 2)

/* Machine Types */
#define EM_PPC64   21

/* Forward declarations */
struct process;

/* Load ELF from file path */
int elf_load(struct process* proc, const char* path, uintptr_t* entry_point);

/* Load ELF from memory buffer */
int elf_load_from_memory(struct process* proc, const void* data, size_t size,
                          uintptr_t* entry_point);

#endif
