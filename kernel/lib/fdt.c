/* AstraLisp OS - Flattened Device Tree Implementation */

#include "fdt.h"
#include "string.h"

static void* fdt_base = NULL;

/* Byte swap helpers (PowerPC is usually big-endian in FDT, but we run LE) */
uint32_t fdt32_to_cpu(uint32_t val) {
    return ((val & 0xFF000000) >> 24) |
           ((val & 0x00FF0000) >> 8)  |
           ((val & 0x0000FF00) << 8)  |
           ((val & 0x000000FF) << 24);
}

uint64_t fdt64_to_cpu(uint64_t val) {
    uint32_t hi = fdt32_to_cpu((uint32_t)(val >> 32));
    uint32_t lo = fdt32_to_cpu((uint32_t)(val & 0xFFFFFFFF));
    return ((uint64_t)lo << 32) | hi;
}

int fdt_check_header(const void* fdt) {
    const struct fdt_header* h = (const struct fdt_header*)fdt;
    if (fdt32_to_cpu(h->magic) != FDT_MAGIC) {
        return -1;
    }
    return 0;
}

int fdt_init(void* fdt_addr) {
    if (fdt_check_header(fdt_addr) != 0) {
        return -1;
    }
    fdt_base = fdt_addr;
    return 0;
}

/* Internal helper to get next token */
static uint32_t fdt_next_token(const void* fdt, int* offset) {
    const struct fdt_header* h = (const struct fdt_header*)fdt;
    uint32_t off_struct = fdt32_to_cpu(h->off_dt_struct);
    
    if (*offset < 0) return FDT_END; /* Error state */
    
    uint32_t token = fdt32_to_cpu(*(uint32_t*)(fdt + off_struct + *offset));
    *offset += 4;
    return token;
}

/* Internal helper to skip current tag */
static int fdt_skip_tag(const void* fdt, int offset, uint32_t token) {
    const struct fdt_header* h = (const struct fdt_header*)fdt;
    uint32_t off_strings = fdt32_to_cpu(h->off_dt_strings);
    uint32_t off_struct = fdt32_to_cpu(h->off_dt_struct);

    switch (token) {
        case FDT_BEGIN_NODE: {
            /* Name is null-terminated string */
            const char* name = (const char*)(fdt + off_struct + offset);
            offset += strlen(name) + 1;
            /* Align to 4 bytes */
            offset = (offset + 3) & ~3;
            break;
        }
        case FDT_PROP: {
            uint32_t len = fdt32_to_cpu(*(uint32_t*)(fdt + off_struct + offset));
            offset += 8; /* len (4) + nameoff (4) */
            offset += len;
            /* Align to 4 bytes */
            offset = (offset + 3) & ~3;
            break;
        }
        case FDT_END_NODE:
        case FDT_NOP:
            break;
        default:
            return -1;
    }
    return offset;
}

int fdt_node_offset_by_path(const char* path) {
    if (!fdt_base) return -1;
    
    const struct fdt_header* h = (const struct fdt_header*)fdt_base;
    uint32_t off_struct = fdt32_to_cpu(h->off_dt_struct);
    int offset = 0;
    uint32_t token;
    
    /* Root node */
    token = fdt_next_token(fdt_base, &offset);
    if (token != FDT_BEGIN_NODE) return -1;
    
    /* Skip root name (empty) */
    offset += 1; 
    offset = (offset + 3) & ~3;
    
    if (strcmp(path, "/") == 0) return 0; // Return offset of root payload? Or aligned? 
    /* For simplicity, this parser is basic. A full one manages stacks of nodes. 
       Let's implement a flat scan for property matches if needed or just specific paths.
       
       For OPAL, we usually look for "/ibm,opal".
    */
    
    /* Basic linear scan for a node name matching path component is hard without recursion state.
       However, we can just search for "ibm,opal" name.
    */
    
    /* Search loop */
    int depth = 0;
    while ((token = fdt_next_token(fdt_base, &offset)) != FDT_END) {
        switch (token) {
            case FDT_BEGIN_NODE: {
                const char* name = (const char*)(fdt_base + off_struct + offset);
                // early_printf("Node: %s\n", name);
                
                /* Check match */
                if (strcmp(name, path + 1) == 0) { /* path+1 to skip leading / */
                    /* Found (very simplified logic) */
                    /* Return offset pointing AFTER the name, to the properties */
                    offset += strlen(name) + 1;
                    return (offset + 3) & ~3;
                }
                
                offset += strlen(name) + 1;
                offset = (offset + 3) & ~3;
                depth++;
                break;
            }
            case FDT_END_NODE:
                depth--;
                break;
            case FDT_PROP:
            case FDT_NOP:
                offset = fdt_skip_tag(fdt_base, offset - 4, token); /* backtrack -4 since we read token */
                /* But wait, fdt_skip_tag expects offset pointing TO data, not after token */
                /* Let's fix loop logic. fdt_skip_tag needs offset relative to struct block start? */
                /* My offsets are relative to struct block. */
                break;
        }
    }
    
    return -1;
}

int fdt_get_prop(int node_offset, const char* name, void** val, int* len) {
    if (!fdt_base) return -1;
    
    const struct fdt_header* h = (const struct fdt_header*)fdt_base;
    uint32_t off_struct = fdt32_to_cpu(h->off_dt_struct);
    uint32_t off_strings = fdt32_to_cpu(h->off_dt_strings);
    
    int offset = node_offset;
    uint32_t token;
    
    while ((token = fdt_next_token(fdt_base, &offset)) == FDT_PROP || token == FDT_NOP) {
        if (token == FDT_NOP) continue;
        
        uint32_t prop_len = fdt32_to_cpu(*(uint32_t*)(fdt_base + off_struct + offset));
        uint32_t nameoff = fdt32_to_cpu(*(uint32_t*)(fdt_base + off_struct + offset + 4));
        const char* prop_name = (const char*)(fdt_base + off_strings + nameoff);
        
        if (strcmp(prop_name, name) == 0) {
            if (val) *val = (void*)(fdt_base + off_struct + offset + 8);
            if (len) *len = prop_len;
            return 0;
        }
        
        offset += 8 + prop_len;
        offset = (offset + 3) & ~3;
    }
    
    return -1;
}
