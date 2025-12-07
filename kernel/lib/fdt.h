/* AstraLisp OS - Flattened Device Tree (FDT) Parser */

#ifndef _KERNEL_FDT_H
#define _KERNEL_FDT_H

#include <stdint.h>
#include <stddef.h>

/* FDT Magic Number (0xD00DFEED big-endian) */
#define FDT_MAGIC 0xD00DFEED

/* FDT Tokens */
#define FDT_BEGIN_NODE  0x00000001
#define FDT_END_NODE    0x00000002
#define FDT_PROP        0x00000003
#define FDT_NOP         0x00000004
#define FDT_END         0x00000009

struct fdt_header {
    uint32_t magic;
    uint32_t totalsize;
    uint32_t off_dt_struct;
    uint32_t off_dt_strings;
    uint32_t off_mem_rsvmap;
    uint32_t version;
    uint32_t last_comp_version;
    uint32_t boot_cpuid_phys;
    uint32_t size_dt_strings;
    uint32_t size_dt_struct;
};

struct fdt_reserve_entry {
    uint64_t address;
    uint64_t size;
};

/* API */
int fdt_init(void* fdt_addr);
int fdt_check_header(const void* fdt);
int fdt_node_offset_by_path(const char* path);
int fdt_get_prop(int node_offset, const char* name, void** val, int* len);
uint32_t fdt_get_phandle(int node_offset);

/* Helper to convert big-endian FDT values to host */
uint32_t fdt32_to_cpu(uint32_t val);
uint64_t fdt64_to_cpu(uint64_t val);

#endif
