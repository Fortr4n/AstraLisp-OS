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
    if (!fdt_base || !path) return -1;
    
    const struct fdt_header* h = (const struct fdt_header*)fdt_base;
    uint32_t off_struct = fdt32_to_cpu(h->off_dt_struct);
    
    /* Start at root */
    if (strcmp(path, "/") == 0) return 0;
    
    int offset = 0;
    uint32_t token;
    
    /* Skip root node begin */
    token = fdt_next_token(fdt_base, &offset);
    if (token != FDT_BEGIN_NODE) return -1;
    
    /* Skip root empty name */
    offset += 1; 
    offset = (offset + 3) & ~3;
    
    /* Parse path components */
    const char* current_path = path;
    if (*current_path == '/') current_path++;
    
    int current_depth = 0;
    int target_depth = 0;
    
    /* Scan sequence matching each component */
    /* Simplified tree traversal: 
       We need to find component X at depth 0, then enter it, find Y at depth 1, etc.
    */
    
    /* Since we don't have a stack, we do linear scan but track depth */
    
    // For now, let's keep the linear scan but ensure we only match at correct depth
    // The previous implementation was too loose.
    // If path is /ibm,opal/console, we need to match ibm,opal at depth 0, enter it, match console at depth 1
    
    /* We will implement a proper segment matcher */
    char segment[64];
    const char* next_slash = strchr(current_path, '/');
    int segment_len = next_slash ? (next_slash - current_path) : strlen(current_path);
    if (segment_len >= 64) return -1;
    strncpy(segment, current_path, segment_len);
    segment[segment_len] = '\0';
    
    int search_depth = 1; /* Children of root are at depth 1 in FDT logic relative to root? Or 0 relative to root content? */
    /* Root is depth 0. Its properties are depth 1? No, structure is:
       NODE
         PROP
         NODE (child)
           PROP
       END_NODE
    */
    
    while ((token = fdt_next_token(fdt_base, &offset)) != FDT_END) {
        switch (token) {
            case FDT_BEGIN_NODE: {
                const char* name = (const char*)(fdt_base + off_struct + offset);
                // early_printf("Node: %s at depth %d\n", name, current_depth);
                
                /* Check if this node matches current segment at correct depth */
                if (current_depth == 0) {
                     if (strcmp(name, segment) == 0) {
                         /* Match found for this component */
                         /* Advance path */
                         if (!next_slash) {
                             /* Full match! */
                             offset += strlen(name) + 1;
                             return (offset + 3) & ~3;
                         }
                         
                         /* Move to next component */
                         current_path = next_slash + 1;
                         next_slash = strchr(current_path, '/');
                         segment_len = next_slash ? (next_slash - current_path) : strlen(current_path);
                         if (segment_len >= 64) return -1;
                         strncpy(segment, current_path, segment_len);
                         segment[segment_len] = '\0';
                         
                         /* We are now looking for match inside this node, which means at next depth */
                         /* Wait, we just entered this node by reading BEGIN_NODE, so depth increments */
                     }
                }
                
                offset += strlen(name) + 1;
                offset = (offset + 3) & ~3;
                current_depth++;
                break;
            }
            case FDT_END_NODE:
                current_depth--;
                if (current_depth < 0) return -1; /* Should not happen */
                break;
            case FDT_PROP: {
                uint32_t len = fdt32_to_cpu(*(uint32_t*)(fdt_base + off_struct + offset));
                offset += 8 + len;
                offset = (offset + 3) & ~3;
                break;
            }
            case FDT_NOP:
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
