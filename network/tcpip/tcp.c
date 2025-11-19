/* AstraLisp OS TCP Implementation */

#include "tcp.h"
#include "../../kernel/mm/heap.h"
#include "../../kernel/driver/network.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

#define TCP_MAX_CONNECTIONS 1024
#define TCP_DEFAULT_MSS 1460
#define TCP_DEFAULT_WINDOW 65535
#define TCP_INITIAL_CWND 1
#define TCP_INITIAL_SSTHRESH 65535
#define TCP_RTO_MIN 1000
#define TCP_RTO_MAX 60000
#define TCP_REASM_QUEUE_MAX_BYTES (256 * 1024)  /* 256KB max out-of-order buffer */

/* Sequence number comparison macros (handle wraparound) */
#define SEQ_LT(a, b)  ((int32_t)((a) - (b)) < 0)
#define SEQ_LEQ(a, b) ((int32_t)((a) - (b)) <= 0)
#define SEQ_GT(a, b)  ((int32_t)((a) - (b)) > 0)
#define SEQ_GEQ(a, b) ((int32_t)((a) - (b)) >= 0)

static struct tcp_connection* connection_list = NULL;
static uint32_t next_local_port = 32768;

/*
 * ============================================================================
 * RED-BLACK TREE IMPLEMENTATION FOR OUT-OF-ORDER REASSEMBLY QUEUE
 * ============================================================================
 */

/* Create reassembly queue */
struct reassembly_queue* reasm_queue_create(size_t max_bytes) {
    struct reassembly_queue* queue = (struct reassembly_queue*)kmalloc(sizeof(struct reassembly_queue));
    if (!queue) {
        return NULL;
    }

    /* Allocate sentinel (nil) node */
    queue->nil = (struct ooo_segment*)kmalloc(sizeof(struct ooo_segment));
    if (!queue->nil) {
        kfree(queue);
        return NULL;
    }

    /* Initialize sentinel */
    memset(queue->nil, 0, sizeof(struct ooo_segment));
    queue->nil->color = RB_BLACK;
    queue->nil->parent = queue->nil;
    queue->nil->left = queue->nil;
    queue->nil->right = queue->nil;

    /* Initialize queue */
    queue->root = queue->nil;
    queue->segment_count = 0;
    queue->total_bytes = 0;
    queue->max_bytes = max_bytes;

    return queue;
}

/* Destroy reassembly queue */
static void reasm_queue_destroy_subtree(struct reassembly_queue* queue, struct ooo_segment* node) {
    if (node == queue->nil) {
        return;
    }

    reasm_queue_destroy_subtree(queue, node->left);
    reasm_queue_destroy_subtree(queue, node->right);

    if (node->data) {
        kfree(node->data);
    }
    kfree(node);
}

void reasm_queue_destroy(struct reassembly_queue* queue) {
    if (!queue) {
        return;
    }

    reasm_queue_destroy_subtree(queue, queue->root);
    kfree(queue->nil);
    kfree(queue);
}

/* Red-Black tree left rotate */
static void rb_left_rotate(struct reassembly_queue* queue, struct ooo_segment* x) {
    struct ooo_segment* y = x->right;  /* Set y */

    /* Turn y's left subtree into x's right subtree */
    x->right = y->left;
    if (y->left != queue->nil) {
        y->left->parent = x;
    }

    /* Link x's parent to y */
    y->parent = x->parent;
    if (x->parent == queue->nil) {
        queue->root = y;
    } else if (x == x->parent->left) {
        x->parent->left = y;
    } else {
        x->parent->right = y;
    }

    /* Put x on y's left */
    y->left = x;
    x->parent = y;
}

/* Red-Black tree right rotate */
static void rb_right_rotate(struct reassembly_queue* queue, struct ooo_segment* x) {
    struct ooo_segment* y = x->left;  /* Set y */

    /* Turn y's right subtree into x's left subtree */
    x->left = y->right;
    if (y->right != queue->nil) {
        y->right->parent = x;
    }

    /* Link x's parent to y */
    y->parent = x->parent;
    if (x->parent == queue->nil) {
        queue->root = y;
    } else if (x == x->parent->right) {
        x->parent->right = y;
    } else {
        x->parent->left = y;
    }

    /* Put x on y's right */
    y->right = x;
    x->parent = y;
}

/* Red-Black tree insert fixup */
static void rb_insert_fixup(struct reassembly_queue* queue, struct ooo_segment* z) {
    while (z->parent->color == RB_RED) {
        if (z->parent == z->parent->parent->left) {
            struct ooo_segment* y = z->parent->parent->right;  /* Uncle */
            if (y->color == RB_RED) {
                /* Case 1: Uncle is red */
                z->parent->color = RB_BLACK;
                y->color = RB_BLACK;
                z->parent->parent->color = RB_RED;
                z = z->parent->parent;
            } else {
                if (z == z->parent->right) {
                    /* Case 2: z is right child */
                    z = z->parent;
                    rb_left_rotate(queue, z);
                }
                /* Case 3: z is left child */
                z->parent->color = RB_BLACK;
                z->parent->parent->color = RB_RED;
                rb_right_rotate(queue, z->parent->parent);
            }
        } else {
            /* Same as above with "left" and "right" exchanged */
            struct ooo_segment* y = z->parent->parent->left;  /* Uncle */
            if (y->color == RB_RED) {
                /* Case 1: Uncle is red */
                z->parent->color = RB_BLACK;
                y->color = RB_BLACK;
                z->parent->parent->color = RB_RED;
                z = z->parent->parent;
            } else {
                if (z == z->parent->left) {
                    /* Case 2: z is left child */
                    z = z->parent;
                    rb_right_rotate(queue, z);
                }
                /* Case 3: z is right child */
                z->parent->color = RB_BLACK;
                z->parent->parent->color = RB_RED;
                rb_left_rotate(queue, z->parent->parent);
            }
        }
    }
    queue->root->color = RB_BLACK;
}

/* Insert segment into reassembly queue */
int reasm_queue_insert(struct reassembly_queue* queue, uint32_t seq_start,
                       uint32_t seq_end, const uint8_t* data, size_t data_len) {
    if (!queue || !data || data_len == 0 || seq_start == seq_end) {
        return -1;
    }

    /* Check buffer limit */
    if (queue->total_bytes + data_len > queue->max_bytes) {
        return -1;  /* Buffer full */
    }

    /* Allocate new node */
    struct ooo_segment* z = (struct ooo_segment*)kmalloc(sizeof(struct ooo_segment));
    if (!z) {
        return -1;
    }

    /* Allocate and copy data */
    z->data = (uint8_t*)kmalloc(data_len);
    if (!z->data) {
        kfree(z);
        return -1;
    }
    memcpy(z->data, data, data_len);

    /* Initialize node */
    z->seq_start = seq_start;
    z->seq_end = seq_end;
    z->data_len = data_len;
    z->timestamp = 0;  /* Would set to current time */
    z->left = queue->nil;
    z->right = queue->nil;
    z->color = RB_RED;

    /* Standard BST insert */
    struct ooo_segment* y = queue->nil;
    struct ooo_segment* x = queue->root;

    while (x != queue->nil) {
        y = x;
        if (SEQ_LT(z->seq_start, x->seq_start)) {
            x = x->left;
        } else {
            x = x->right;
        }
    }

    z->parent = y;

    if (y == queue->nil) {
        queue->root = z;
    } else if (SEQ_LT(z->seq_start, y->seq_start)) {
        y->left = z;
    } else {
        y->right = z;
    }

    /* Fix Red-Black tree properties */
    rb_insert_fixup(queue, z);

    /* Update statistics */
    queue->segment_count++;
    queue->total_bytes += data_len;

    return 0;
}

/* Find minimum node in subtree */
static struct ooo_segment* rb_minimum(struct reassembly_queue* queue, struct ooo_segment* x) {
    while (x->left != queue->nil) {
        x = x->left;
    }
    return x;
}

/* Red-Black tree transplant */
static void rb_transplant(struct reassembly_queue* queue, struct ooo_segment* u,
                         struct ooo_segment* v) {
    if (u->parent == queue->nil) {
        queue->root = v;
    } else if (u == u->parent->left) {
        u->parent->left = v;
    } else {
        u->parent->right = v;
    }
    v->parent = u->parent;
}

/* Red-Black tree delete fixup */
static void rb_delete_fixup(struct reassembly_queue* queue, struct ooo_segment* x) {
    while (x != queue->root && x->color == RB_BLACK) {
        if (x == x->parent->left) {
            struct ooo_segment* w = x->parent->right;  /* Sibling */
            if (w->color == RB_RED) {
                /* Case 1: Sibling is red */
                w->color = RB_BLACK;
                x->parent->color = RB_RED;
                rb_left_rotate(queue, x->parent);
                w = x->parent->right;
            }
            if (w->left->color == RB_BLACK && w->right->color == RB_BLACK) {
                /* Case 2: Sibling's children are both black */
                w->color = RB_RED;
                x = x->parent;
            } else {
                if (w->right->color == RB_BLACK) {
                    /* Case 3: Sibling's right child is black */
                    w->left->color = RB_BLACK;
                    w->color = RB_RED;
                    rb_right_rotate(queue, w);
                    w = x->parent->right;
                }
                /* Case 4: Sibling's right child is red */
                w->color = x->parent->color;
                x->parent->color = RB_BLACK;
                w->right->color = RB_BLACK;
                rb_left_rotate(queue, x->parent);
                x = queue->root;
            }
        } else {
            /* Same as above with "left" and "right" exchanged */
            struct ooo_segment* w = x->parent->left;  /* Sibling */
            if (w->color == RB_RED) {
                /* Case 1: Sibling is red */
                w->color = RB_BLACK;
                x->parent->color = RB_RED;
                rb_right_rotate(queue, x->parent);
                w = x->parent->left;
            }
            if (w->right->color == RB_BLACK && w->left->color == RB_BLACK) {
                /* Case 2: Sibling's children are both black */
                w->color = RB_RED;
                x = x->parent;
            } else {
                if (w->left->color == RB_BLACK) {
                    /* Case 3: Sibling's left child is black */
                    w->right->color = RB_BLACK;
                    w->color = RB_RED;
                    rb_left_rotate(queue, w);
                    w = x->parent->left;
                }
                /* Case 4: Sibling's left child is red */
                w->color = x->parent->color;
                x->parent->color = RB_BLACK;
                w->left->color = RB_BLACK;
                rb_right_rotate(queue, x->parent);
                x = queue->root;
            }
        }
    }
    x->color = RB_BLACK;
}

/* Delete node from Red-Black tree */
static void rb_delete(struct reassembly_queue* queue, struct ooo_segment* z) {
    struct ooo_segment* y = z;
    struct ooo_segment* x;
    rb_color_t y_original_color = y->color;

    if (z->left == queue->nil) {
        x = z->right;
        rb_transplant(queue, z, z->right);
    } else if (z->right == queue->nil) {
        x = z->left;
        rb_transplant(queue, z, z->left);
    } else {
        y = rb_minimum(queue, z->right);
        y_original_color = y->color;
        x = y->right;
        if (y->parent == z) {
            x->parent = y;
        } else {
            rb_transplant(queue, y, y->right);
            y->right = z->right;
            y->right->parent = y;
        }
        rb_transplant(queue, z, y);
        y->left = z->left;
        y->left->parent = y;
        y->color = z->color;
    }

    if (y_original_color == RB_BLACK) {
        rb_delete_fixup(queue, x);
    }

    /* Update statistics */
    queue->segment_count--;
    queue->total_bytes -= z->data_len;

    /* Free node */
    if (z->data) {
        kfree(z->data);
    }
    kfree(z);
}

/* Extract ready segments (those starting at recv_next) */
struct ooo_segment* reasm_queue_extract_ready(struct reassembly_queue* queue,
                                              uint32_t recv_next) {
    if (!queue || queue->root == queue->nil) {
        return NULL;
    }

    /* Find leftmost segment */
    struct ooo_segment* node = rb_minimum(queue, queue->root);

    /* Check if it starts at recv_next */
    if (node->seq_start != recv_next) {
        return NULL;  /* Gap exists */
    }

    /* Extract the segment */
    struct ooo_segment* result = (struct ooo_segment*)kmalloc(sizeof(struct ooo_segment));
    if (!result) {
        return NULL;
    }

    /* Copy segment data */
    *result = *node;
    result->data = (uint8_t*)kmalloc(node->data_len);
    if (!result->data) {
        kfree(result);
        return NULL;
    }
    memcpy(result->data, node->data, node->data_len);

    /* Remove from tree */
    rb_delete(queue, node);

    /* Mark as standalone node */
    result->left = NULL;
    result->right = NULL;
    result->parent = NULL;

    return result;
}

/* Remove all segments with sequence numbers below seq */
void reasm_queue_remove_below(struct reassembly_queue* queue, uint32_t seq) {
    if (!queue) {
        return;
    }

    /* Traverse tree and collect nodes to delete */
    struct ooo_segment* to_delete[1024];  /* Max segments to delete at once */
    uint32_t delete_count = 0;

    struct ooo_segment* node = queue->root;
    while (node != queue->nil && delete_count < 1024) {
        if (SEQ_LT(node->seq_end, seq)) {
            /* This entire segment is below seq */
            to_delete[delete_count++] = node;
            node = node->right;
        } else if (SEQ_LT(node->seq_start, seq)) {
            /* Partial overlap - shouldn't normally happen */
            to_delete[delete_count++] = node;
            node = node->right;
        } else {
            /* This segment is above seq, check left subtree */
            node = node->left;
        }
    }

    /* Delete collected nodes */
    for (uint32_t i = 0; i < delete_count; i++) {
        rb_delete(queue, to_delete[i]);
    }
}

/*
 * ============================================================================
 * SACK (SELECTIVE ACKNOWLEDGMENT) IMPLEMENTATION - RFC 2018
 * ============================================================================
 */

/* Generate SACK blocks from reassembly queue */
void tcp_generate_sack_blocks(struct tcp_connection* conn) {
    if (!conn || !conn->reasm_queue || !conn->sack_permitted) {
        return;
    }

    struct reassembly_queue* queue = conn->reasm_queue;
    conn->sack_block_count = 0;

    if (queue->root == queue->nil) {
        return;  /* No out-of-order segments */
    }

    /* In-order traversal to collect contiguous blocks */
    struct ooo_segment* stack[256];
    int stack_top = -1;
    struct ooo_segment* current = queue->root;

    uint32_t current_left = 0;
    uint32_t current_right = 0;
    bool in_block = false;

    /* In-order traversal */
    while (stack_top >= 0 || current != queue->nil) {
        if (current != queue->nil) {
            stack[++stack_top] = current;
            current = current->left;
        } else {
            current = stack[stack_top--];

            if (!in_block) {
                /* Start new block */
                current_left = current->seq_start;
                current_right = current->seq_end;
                in_block = true;
            } else {
                /* Check if contiguous with current block */
                if (current->seq_start == current_right) {
                    /* Extend block */
                    current_right = current->seq_end;
                } else {
                    /* Gap found - save current block */
                    if (conn->sack_block_count < TCP_MAX_SACK_BLOCKS) {
                        conn->sack_blocks[conn->sack_block_count].left_edge = current_left;
                        conn->sack_blocks[conn->sack_block_count].right_edge = current_right;
                        conn->sack_block_count++;
                    }

                    /* Start new block */
                    current_left = current->seq_start;
                    current_right = current->seq_end;
                }
            }

            current = current->right;
        }
    }

    /* Save last block */
    if (in_block && conn->sack_block_count < TCP_MAX_SACK_BLOCKS) {
        conn->sack_blocks[conn->sack_block_count].left_edge = current_left;
        conn->sack_blocks[conn->sack_block_count].right_edge = current_right;
        conn->sack_block_count++;
    }
}

/* Add SACK option to TCP header */
int tcp_add_sack_option(struct tcp_header* header, const struct tcp_sack_block* blocks,
                        uint32_t block_count) {
    if (!header || !blocks || block_count == 0 || block_count > TCP_MAX_SACK_BLOCKS) {
        return -1;
    }

    uint8_t* options = header->options;
    size_t offset = 0;

    /* Find end of existing options */
    while (offset < 40 && options[offset] != TCP_OPT_END) {
        if (options[offset] == TCP_OPT_NOP) {
            offset++;
        } else {
            uint8_t len = options[offset + 1];
            if (len < 2 || offset + len > 40) {
                break;
            }
            offset += len;
        }
    }

    /* Calculate SACK option length */
    size_t sack_len = 2 + (block_count * 8);  /* Kind + Length + (8 bytes per block) */

    if (offset + sack_len > 40) {
        return -1;  /* Not enough space */
    }

    /* Add SACK option */
    options[offset++] = TCP_OPT_SACK;
    options[offset++] = (uint8_t)sack_len;

    for (uint32_t i = 0; i < block_count; i++) {
        /* Left edge (4 bytes, network byte order) */
        uint32_t left = blocks[i].left_edge;
        options[offset++] = (left >> 24) & 0xFF;
        options[offset++] = (left >> 16) & 0xFF;
        options[offset++] = (left >> 8) & 0xFF;
        options[offset++] = left & 0xFF;

        /* Right edge (4 bytes, network byte order) */
        uint32_t right = blocks[i].right_edge;
        options[offset++] = (right >> 24) & 0xFF;
        options[offset++] = (right >> 16) & 0xFF;
        options[offset++] = (right >> 8) & 0xFF;
        options[offset++] = right & 0xFF;
    }

    /* Add END option */
    if (offset < 40) {
        options[offset] = TCP_OPT_END;
    }

    /* Update data offset (in 32-bit words) */
    header->data_offset = 5 + ((offset + 3) / 4);

    return 0;
}

/* Parse SACK option from TCP header */
int tcp_parse_sack_option(const uint8_t* options, size_t options_len,
                          struct tcp_sack_block* blocks, uint32_t* block_count) {
    if (!options || !blocks || !block_count) {
        return -1;
    }

    *block_count = 0;
    size_t offset = 0;

    while (offset < options_len) {
        uint8_t kind = options[offset];

        if (kind == TCP_OPT_END) {
            break;
        }

        if (kind == TCP_OPT_NOP) {
            offset++;
            continue;
        }

        if (offset + 1 >= options_len) {
            break;  /* Invalid option */
        }

        uint8_t len = options[offset + 1];
        if (len < 2 || offset + len > options_len) {
            break;  /* Invalid length */
        }

        if (kind == TCP_OPT_SACK) {
            /* Parse SACK blocks */
            size_t block_bytes = len - 2;
            if (block_bytes % 8 != 0) {
                return -1;  /* Invalid SACK option */
            }

            uint32_t num_blocks = block_bytes / 8;
            if (num_blocks > TCP_MAX_SACK_BLOCKS) {
                num_blocks = TCP_MAX_SACK_BLOCKS;
            }

            for (uint32_t i = 0; i < num_blocks; i++) {
                size_t block_offset = offset + 2 + (i * 8);

                /* Parse left edge */
                uint32_t left = ((uint32_t)options[block_offset] << 24) |
                               ((uint32_t)options[block_offset + 1] << 16) |
                               ((uint32_t)options[block_offset + 2] << 8) |
                               ((uint32_t)options[block_offset + 3]);

                /* Parse right edge */
                uint32_t right = ((uint32_t)options[block_offset + 4] << 24) |
                                ((uint32_t)options[block_offset + 5] << 16) |
                                ((uint32_t)options[block_offset + 6] << 8) |
                                ((uint32_t)options[block_offset + 7]);

                blocks[*block_count].left_edge = left;
                blocks[*block_count].right_edge = right;
                (*block_count)++;
            }

            return 0;  /* SACK option found and parsed */
        }

        offset += len;
    }

    return -1;  /* SACK option not found */
}

/*
 * ============================================================================
 * SYN COOKIES IMPLEMENTATION - RFC 4987
 * ============================================================================
 */

#define SYN_COOKIE_SECRET_SIZE 32
#define SYN_COOKIE_TIMESTAMP_BITS 5
#define SYN_COOKIE_MSS_BITS 3

/* Secret keys for SYN cookie generation (should be rotated periodically) */
static uint32_t syn_cookie_secret[SYN_COOKIE_SECRET_SIZE / 4] = {
    0xDEADBEEF, 0xCAFEBABE, 0xFEEDFACE, 0xBAADF00D,
    0xC0FFEE00, 0xDEADC0DE, 0xFACEFEED, 0xBEEFFACE
};

/* Current time counter for SYN cookies (incremented every 64 seconds) */
static uint32_t syn_cookie_time = 0;

/* MSS encoding table (limited values for 3-bit encoding) */
static const uint16_t mss_table[] = {
    536,   /* Code 0: Minimum MSS */
    1200,  /* Code 1 */
    1300,  /* Code 2 */
    1400,  /* Code 3 */
    1440,  /* Code 4: Ethernet - headers */
    1460,  /* Code 5: Default MSS */
    4312,  /* Code 6: Jumbo frames */
    8960   /* Code 7: Large MTU */
};

/* Encode MSS value to 3-bit code */
uint8_t tcp_encode_mss(uint16_t mss) {
    /* Find closest MSS value in table */
    uint8_t best_code = 0;
    uint16_t best_diff = 0xFFFF;

    for (uint8_t i = 0; i < 8; i++) {
        uint16_t diff = (mss > mss_table[i]) ? (mss - mss_table[i]) : (mss_table[i] - mss);
        if (diff < best_diff) {
            best_diff = diff;
            best_code = i;
        }
    }

    return best_code;
}

/* Decode 3-bit MSS code to value */
uint16_t tcp_decode_mss(uint8_t mss_code) {
    if (mss_code >= 8) {
        return mss_table[5];  /* Default MSS */
    }
    return mss_table[mss_code];
}

/* SipHash-2-4 for cookie generation (simplified version) */
static uint64_t siphash24(const uint8_t* data, size_t len, const uint8_t* key) {
    uint64_t v0 = 0x736f6d6570736575ULL;
    uint64_t v1 = 0x646f72616e646f6dULL;
    uint64_t v2 = 0x6c7967656e657261ULL;
    uint64_t v3 = 0x7465646279746573ULL;

    /* Initialize with key */
    uint64_t k0 = ((uint64_t)key[0]) | ((uint64_t)key[1] << 8) |
                  ((uint64_t)key[2] << 16) | ((uint64_t)key[3] << 24) |
                  ((uint64_t)key[4] << 32) | ((uint64_t)key[5] << 40) |
                  ((uint64_t)key[6] << 48) | ((uint64_t)key[7] << 56);

    uint64_t k1 = ((uint64_t)key[8]) | ((uint64_t)key[9] << 8) |
                  ((uint64_t)key[10] << 16) | ((uint64_t)key[11] << 24) |
                  ((uint64_t)key[12] << 32) | ((uint64_t)key[13] << 40) |
                  ((uint64_t)key[14] << 48) | ((uint64_t)key[15] << 56);

    v3 ^= k1;
    v2 ^= k0;
    v1 ^= k1;
    v0 ^= k0;

    /* Process message */
    const uint8_t* end = data + len - (len % 8);
    const uint8_t* ptr = data;

    while (ptr < end) {
        uint64_t m = ((uint64_t)ptr[0]) | ((uint64_t)ptr[1] << 8) |
                     ((uint64_t)ptr[2] << 16) | ((uint64_t)ptr[3] << 24) |
                     ((uint64_t)ptr[4] << 32) | ((uint64_t)ptr[5] << 40) |
                     ((uint64_t)ptr[6] << 48) | ((uint64_t)ptr[7] << 56);

        v3 ^= m;

        /* SipRound (simplified) */
        for (int i = 0; i < 2; i++) {
            v0 += v1; v1 = (v1 << 13) | (v1 >> 51); v1 ^= v0; v0 = (v0 << 32) | (v0 >> 32);
            v2 += v3; v3 = (v3 << 16) | (v3 >> 48); v3 ^= v2;
            v0 += v3; v3 = (v3 << 21) | (v3 >> 43); v3 ^= v0;
            v2 += v1; v1 = (v1 << 17) | (v1 >> 47); v1 ^= v2; v2 = (v2 << 32) | (v2 >> 32);
        }

        v0 ^= m;
        ptr += 8;
    }

    /* Finalization */
    v2 ^= 0xff;
    for (int i = 0; i < 4; i++) {
        v0 += v1; v1 = (v1 << 13) | (v1 >> 51); v1 ^= v0; v0 = (v0 << 32) | (v0 >> 32);
        v2 += v3; v3 = (v3 << 16) | (v3 >> 48); v3 ^= v2;
        v0 += v3; v3 = (v3 << 21) | (v3 >> 43); v3 ^= v0;
        v2 += v1; v1 = (v1 << 17) | (v1 >> 47); v1 ^= v2; v2 = (v2 << 32) | (v2 >> 32);
    }

    return v0 ^ v1 ^ v2 ^ v3;
}

/* Generate SYN cookie for connection */
uint32_t tcp_generate_syn_cookie(uint32_t src_addr, uint32_t dst_addr,
                                  uint16_t src_port, uint16_t dst_port,
                                  uint32_t seq_num, uint8_t mss_code) {
    /* Encode connection tuple */
    uint8_t data[20];
    data[0] = (src_addr >> 24) & 0xFF;
    data[1] = (src_addr >> 16) & 0xFF;
    data[2] = (src_addr >> 8) & 0xFF;
    data[3] = src_addr & 0xFF;
    data[4] = (dst_addr >> 24) & 0xFF;
    data[5] = (dst_addr >> 16) & 0xFF;
    data[6] = (dst_addr >> 8) & 0xFF;
    data[7] = dst_addr & 0xFF;
    data[8] = (src_port >> 8) & 0xFF;
    data[9] = src_port & 0xFF;
    data[10] = (dst_port >> 8) & 0xFF;
    data[11] = dst_port & 0xFF;
    data[12] = (syn_cookie_time >> 24) & 0xFF;
    data[13] = (syn_cookie_time >> 16) & 0xFF;
    data[14] = (syn_cookie_time >> 8) & 0xFF;
    data[15] = syn_cookie_time & 0xFF;
    data[16] = mss_code & 0x07;
    data[17] = (seq_num >> 16) & 0xFF;
    data[18] = (seq_num >> 8) & 0xFF;
    data[19] = seq_num & 0xFF;

    /* Generate hash using SipHash */
    uint64_t hash = siphash24(data, sizeof(data), (uint8_t*)syn_cookie_secret);

    /* Construct cookie:
     * Bits 31-27: Timestamp (5 bits, wraps every ~2048 seconds at 64s granularity)
     * Bits 26-24: MSS code (3 bits)
     * Bits 23-0:  Hash (24 bits)
     */
    uint32_t cookie = 0;
    cookie |= ((syn_cookie_time & 0x1F) << 27);           /* 5-bit timestamp */
    cookie |= ((mss_code & 0x07) << 24);                  /* 3-bit MSS code */
    cookie |= ((uint32_t)(hash & 0xFFFFFF));              /* 24-bit hash */

    return cookie;
}

/* Validate SYN cookie */
bool tcp_validate_syn_cookie(uint32_t src_addr, uint32_t dst_addr,
                              uint16_t src_port, uint16_t dst_port,
                              uint32_t seq_num, uint32_t cookie, uint8_t* mss_code) {
    if (!mss_code) {
        return false;
    }

    /* Extract fields from cookie */
    uint32_t cookie_time = (cookie >> 27) & 0x1F;
    uint8_t cookie_mss = (cookie >> 24) & 0x07;
    uint32_t cookie_hash = cookie & 0xFFFFFF;

    /* Check timestamp (allow ±4 time units = ±256 seconds tolerance) */
    uint32_t current_time = syn_cookie_time & 0x1F;
    uint32_t time_diff = (current_time >= cookie_time) ?
                         (current_time - cookie_time) :
                         (32 + current_time - cookie_time);

    if (time_diff > 4) {
        return false;  /* Cookie too old or from future */
    }

    /* Reconstruct hash with extracted timestamp */
    uint8_t data[20];
    data[0] = (src_addr >> 24) & 0xFF;
    data[1] = (src_addr >> 16) & 0xFF;
    data[2] = (src_addr >> 8) & 0xFF;
    data[3] = src_addr & 0xFF;
    data[4] = (dst_addr >> 24) & 0xFF;
    data[5] = (dst_addr >> 16) & 0xFF;
    data[6] = (dst_addr >> 8) & 0xFF;
    data[7] = dst_addr & 0xFF;
    data[8] = (src_port >> 8) & 0xFF;
    data[9] = src_port & 0xFF;
    data[10] = (dst_port >> 8) & 0xFF;
    data[11] = dst_port & 0xFF;

    /* Try with current time and recent past times */
    for (int t = 0; t <= 4; t++) {
        uint32_t test_time = (syn_cookie_time - t) & 0x1F;
        if (test_time != cookie_time) {
            continue;
        }

        data[12] = (test_time >> 24) & 0xFF;
        data[13] = (test_time >> 16) & 0xFF;
        data[14] = (test_time >> 8) & 0xFF;
        data[15] = test_time & 0xFF;
        data[16] = cookie_mss & 0x07;
        data[17] = (seq_num >> 16) & 0xFF;
        data[18] = (seq_num >> 8) & 0xFF;
        data[19] = seq_num & 0xFF;

        /* Generate hash */
        uint64_t hash = siphash24(data, sizeof(data), (uint8_t*)syn_cookie_secret);
        uint32_t expected_hash = (uint32_t)(hash & 0xFFFFFF);

        if (expected_hash == cookie_hash) {
            /* Valid cookie */
            *mss_code = cookie_mss;
            return true;
        }
    }

    return false;  /* Invalid cookie */
}

/* Increment SYN cookie time counter (called every 64 seconds) */
void tcp_syn_cookie_tick(void) {
    syn_cookie_time++;
}

/* Calculate TCP checksum */
uint16_t tcp_calculate_checksum(const struct tcp_header* header, const uint8_t* data,
                                size_t data_len, uint32_t src_addr, uint32_t dst_addr) {
    uint32_t sum = 0;
    
    /* Pseudo header */
    sum += (src_addr >> 16) + (src_addr & 0xFFFF);
    sum += (dst_addr >> 16) + (dst_addr & 0xFFFF);
    sum += 6;  /* TCP protocol */
    sum += (uint16_t)(sizeof(struct tcp_header) + data_len);
    
    /* TCP header */
    const uint16_t* hdr_words = (const uint16_t*)header;
    for (size_t i = 0; i < sizeof(struct tcp_header) / 2; i++) {
        if (i != 8) {  /* Skip checksum field */
            sum += hdr_words[i];
        }
    }
    
    /* Data */
    const uint16_t* data_words = (const uint16_t*)data;
    for (size_t i = 0; i < data_len / 2; i++) {
        sum += data_words[i];
    }
    
    if (data_len % 2) {
        sum += ((uint16_t)data[data_len - 1]) << 8;
    }
    
    /* Fold to 16 bits */
    while (sum >> 16) {
        sum = (sum & 0xFFFF) + (sum >> 16);
    }
    
    return ~sum;
}

/* Send TCP segment */
static int tcp_send_segment(struct tcp_connection* conn, uint8_t flags,
                            const uint8_t* data, size_t data_len) {
    if (!conn) {
        return -1;
    }
    
    /* Allocate segment */
    struct tcp_segment* segment = (struct tcp_segment*)kmalloc(sizeof(struct tcp_segment));
    if (!segment) {
        return -1;
    }
    
    memset(&segment->header, 0, sizeof(struct tcp_header));
    segment->header.src_port = conn->local_port;
    segment->header.dst_port = conn->remote_port;
    segment->header.seq_num = conn->send_seq;
    segment->header.ack_num = conn->recv_next;
    segment->header.data_offset = 5;  /* 20 bytes header (may be updated by options) */
    segment->header.flags = flags;
    segment->header.window = conn->recv_window;
    segment->header.urgent_ptr = 0;

    /* Add SACK options if this is an ACK and we have SACK blocks */
    if ((flags & TCP_FLAG_ACK) && conn->sack_permitted && conn->sack_block_count > 0) {
        tcp_add_sack_option(&segment->header, conn->sack_blocks, conn->sack_block_count);
    }
    
    if (data && data_len > 0) {
        segment->data = (uint8_t*)kmalloc(data_len);
        if (!segment->data) {
            kfree(segment);
            return -1;
        }
        memcpy(segment->data, data, data_len);
        segment->data_len = data_len;
    } else {
        segment->data = NULL;
        segment->data_len = 0;
    }
    
    /* Calculate checksum */
    segment->header.checksum = tcp_calculate_checksum(&segment->header, segment->data,
                                                       segment->data_len,
                                                       conn->local_addr, conn->remote_addr);
    
    /* Add to send queue */
    segment->next = conn->send_queue;
    conn->send_queue = segment;
    
    /* Update sequence number */
    if (flags & TCP_FLAG_SYN) {
        conn->send_seq++;
    }
    if (flags & TCP_FLAG_FIN) {
        conn->send_seq++;
    }
    if (data_len > 0) {
        conn->send_seq += data_len;
    }
    
    conn->send_next = conn->send_seq;
    
    /* Send via network interface */
    /* This would call the network driver */
    
    conn->segments_sent++;
    if (data_len > 0) {
        conn->bytes_sent += data_len;
    }
    
    return 0;
}

/* Handle state transitions */
int tcp_handle_state_transition(struct tcp_connection* conn, const struct tcp_header* header) {
    if (!conn || !header) {
        return -1;
    }
    
    uint8_t flags = header->flags;
    
    switch (conn->state) {
        case TCP_CLOSED:
            /* Connection closed, ignore */
            return -1;
            
        case TCP_LISTEN:
            if (flags & TCP_FLAG_SYN) {
                /* SYN received - send SYN-ACK */
                conn->state = TCP_SYN_RECEIVED;
                conn->recv_seq = header->seq_num;
                conn->recv_next = conn->recv_seq + 1;
                tcp_send_segment(conn, TCP_FLAG_SYN | TCP_FLAG_ACK, NULL, 0);
                return 0;
            }
            break;
            
        case TCP_SYN_SENT:
            if (flags & TCP_FLAG_SYN && flags & TCP_FLAG_ACK) {
                /* SYN-ACK received */
                conn->state = TCP_ESTABLISHED;
                conn->recv_seq = header->seq_num;
                conn->recv_next = conn->recv_seq + 1;
                conn->send_una = header->ack_num;
                tcp_send_segment(conn, TCP_FLAG_ACK, NULL, 0);
                return 0;
            } else if (flags & TCP_FLAG_SYN) {
                /* Simultaneous open */
                conn->state = TCP_SYN_RECEIVED;
                conn->recv_seq = header->seq_num;
                conn->recv_next = conn->recv_seq + 1;
                tcp_send_segment(conn, TCP_FLAG_SYN | TCP_FLAG_ACK, NULL, 0);
                return 0;
            }
            break;
            
        case TCP_SYN_RECEIVED:
            if (flags & TCP_FLAG_ACK) {
                /* ACK received - connection established */
                conn->state = TCP_ESTABLISHED;
                conn->send_una = header->ack_num;
                return 0;
            }
            break;
            
        case TCP_ESTABLISHED:
            if (flags & TCP_FLAG_FIN) {
                /* FIN received */
                conn->state = TCP_CLOSE_WAIT;
                conn->recv_next++;
                tcp_send_segment(conn, TCP_FLAG_ACK, NULL, 0);
                return 0;
            }
            break;
            
        case TCP_FIN_WAIT_1:
            if (flags & TCP_FLAG_ACK) {
                /* ACK for our FIN */
                conn->state = TCP_FIN_WAIT_2;
                return 0;
            } else if (flags & TCP_FLAG_FIN) {
                /* Simultaneous close */
                conn->state = TCP_CLOSING;
                conn->recv_next++;
                tcp_send_segment(conn, TCP_FLAG_ACK, NULL, 0);
                return 0;
            }
            break;
            
        case TCP_FIN_WAIT_2:
            if (flags & TCP_FLAG_FIN) {
                /* FIN received */
                conn->state = TCP_TIME_WAIT;
                conn->recv_next++;
                tcp_send_segment(conn, TCP_FLAG_ACK, NULL, 0);
                return 0;
            }
            break;
            
        case TCP_CLOSE_WAIT:
            /* Application should close */
            break;
            
        case TCP_CLOSING:
            if (flags & TCP_FLAG_ACK) {
                /* ACK for our FIN */
                conn->state = TCP_TIME_WAIT;
                return 0;
            }
            break;
            
        case TCP_LAST_ACK:
            if (flags & TCP_FLAG_ACK) {
                /* ACK for our FIN - connection closed */
                conn->state = TCP_CLOSED;
                return 0;
            }
            break;
            
        case TCP_TIME_WAIT:
            /* Wait for 2MSL */
            break;
            
        default:
            break;
    }
    
    return 0;
}

/* Update congestion window (TCP Reno) */
void tcp_update_cwnd(struct tcp_connection* conn, bool ack_received, bool timeout) {
    if (!conn) {
        return;
    }
    
    if (timeout) {
        /* Timeout - enter slow start */
        conn->ssthresh = conn->cwnd / 2;
        if (conn->ssthresh < 1) {
            conn->ssthresh = 1;
        }
        conn->cwnd = TCP_INITIAL_CWND;
        conn->in_slow_start = true;
        conn->in_congestion_avoidance = false;
        conn->retransmissions++;
        return;
    }
    
    if (ack_received) {
        if (conn->in_slow_start) {
            /* Slow start: exponential growth */
            conn->cwnd++;
            if (conn->cwnd >= conn->ssthresh) {
                conn->in_slow_start = false;
                conn->in_congestion_avoidance = true;
            }
        } else if (conn->in_congestion_avoidance) {
            /* Congestion avoidance: linear growth (additive increase) */
            /* cwnd += 1/cwnd per ACK, so we increment by 1 every cwnd ACKs */
            static uint32_t ack_counter = 0;
            ack_counter++;
            if (ack_counter >= conn->cwnd) {
                conn->cwnd++;
                ack_counter = 0;
            }
        }
    } else {
        /* Duplicate ACK - fast retransmit */
        conn->dup_ack_count++;
        if (conn->dup_ack_count == 3) {
            /* Fast retransmit */
            conn->ssthresh = conn->cwnd / 2;
            if (conn->ssthresh < 1) {
                conn->ssthresh = 1;
            }
            conn->cwnd = conn->ssthresh + 3;
            conn->in_slow_start = false;
            conn->in_congestion_avoidance = true;
            tcp_retransmit(conn);
            conn->retransmissions++;
        }
    }
}

/* Retransmit segment */
int tcp_retransmit(struct tcp_connection* conn) {
    if (!conn) {
        return -1;
    }
    
    /* Retransmit oldest unacked segment */
    struct tcp_segment* segment = conn->send_unacked;
    if (!segment) {
        return -1;
    }
    
    /* Resend segment */
    /* This would call tcp_send_segment with the same data */
    
    /* Update retransmit timeout (exponential backoff) */
    conn->retransmit_timeout *= 2;
    if (conn->retransmit_timeout > TCP_RTO_MAX) {
        conn->retransmit_timeout = TCP_RTO_MAX;
    }
    
    return 0;
}

/* Process incoming TCP segment */
int tcp_process_segment(struct tcp_connection* conn, const struct tcp_header* header,
                        const uint8_t* data, size_t data_len) {
    if (!conn || !header) {
        return -1;
    }
    
    /* Verify checksum */
    uint16_t calculated_checksum = tcp_calculate_checksum(header, data, data_len,
                                                          conn->remote_addr, conn->local_addr);
    if (calculated_checksum != header->checksum) {
        return -1;  /* Invalid checksum */
    }
    
    /* Update receive window */
    conn->recv_window = header->window;
    
    /* Handle ACK */
    if (header->flags & TCP_FLAG_ACK) {
        if (header->ack_num > conn->send_una) {
            /* New ACK */
            uint32_t acked_bytes = header->ack_num - conn->send_una;
            conn->send_una = header->ack_num;
            conn->send_buffer_used -= acked_bytes;
            
            /* Remove acked segments from unacked queue */
            struct tcp_segment* seg = conn->send_unacked;
            struct tcp_segment* prev = NULL;
            while (seg) {
                if (seg->header.seq_num + seg->data_len <= conn->send_una) {
                    /* Segment fully acked */
                    struct tcp_segment* next = seg->next;
                    if (prev) {
                        prev->next = next;
                    } else {
                        conn->send_unacked = next;
                    }
                    if (seg->data) {
                        kfree(seg->data);
                    }
                    kfree(seg);
                    seg = next;
                } else {
                    prev = seg;
                    seg = seg->next;
                }
            }
            
            /* Update congestion window */
            tcp_update_cwnd(conn, true, false);
            conn->dup_ack_count = 0;
        } else if (header->ack_num == conn->send_una) {
            /* Duplicate ACK */
            conn->dup_ack_count++;
            tcp_update_cwnd(conn, false, false);
        }
    }
    
    /* Handle data */
    if (data_len > 0) {
        uint32_t seq_start = header->seq_num;
        uint32_t seq_end = seq_start + data_len;

        if (seq_start == conn->recv_next) {
            /* In-order data - add to receive queue */
            struct tcp_segment* segment = (struct tcp_segment*)kmalloc(sizeof(struct tcp_segment));
            if (segment) {
                memcpy(&segment->header, header, sizeof(struct tcp_header));
                segment->data = (uint8_t*)kmalloc(data_len);
                if (segment->data) {
                    memcpy(segment->data, data, data_len);
                    segment->data_len = data_len;
                    segment->next = conn->recv_queue;
                    conn->recv_queue = segment;
                    conn->recv_next = seq_end;
                    conn->recv_buffer_used += data_len;
                    conn->bytes_received += data_len;

                    /* Check reassembly queue for segments that are now in-order */
                    if (conn->reasm_queue) {
                        struct ooo_segment* ooo_seg = reasm_queue_extract_ready(
                            conn->reasm_queue, conn->recv_next);

                        while (ooo_seg) {
                            /* Add to receive queue */
                            struct tcp_segment* in_order_seg = (struct tcp_segment*)kmalloc(
                                sizeof(struct tcp_segment));
                            if (in_order_seg) {
                                memset(&in_order_seg->header, 0, sizeof(struct tcp_header));
                                in_order_seg->data = ooo_seg->data;
                                in_order_seg->data_len = ooo_seg->data_len;
                                in_order_seg->next = conn->recv_queue;
                                conn->recv_queue = in_order_seg;
                                conn->recv_next = ooo_seg->seq_end;
                                conn->recv_buffer_used += ooo_seg->data_len;
                                conn->bytes_received += ooo_seg->data_len;

                                /* Don't free data - transferred to receive queue */
                                kfree(ooo_seg);
                            } else {
                                /* Failed to allocate - free segment */
                                if (ooo_seg->data) {
                                    kfree(ooo_seg->data);
                                }
                                kfree(ooo_seg);
                                break;
                            }

                            /* Try next segment */
                            ooo_seg = reasm_queue_extract_ready(conn->reasm_queue,
                                                                conn->recv_next);
                        }
                    }
                } else {
                    kfree(segment);
                }
            }

            /* Generate SACK blocks and send ACK */
            if (conn->sack_permitted) {
                tcp_generate_sack_blocks(conn);
            }
            tcp_send_segment(conn, TCP_FLAG_ACK, NULL, 0);

        } else if (SEQ_GT(seq_start, conn->recv_next)) {
            /* Out-of-order data - add to reassembly queue */
            if (conn->reasm_queue) {
                /* Check for duplicate or overlapping segment */
                if (SEQ_LEQ(seq_end, conn->recv_next)) {
                    /* Old duplicate - ignore */
                } else {
                    /* Trim overlap with already received data */
                    uint32_t trim_start = seq_start;
                    const uint8_t* trim_data = data;
                    size_t trim_len = data_len;

                    if (SEQ_LT(seq_start, conn->recv_next)) {
                        /* Partial overlap with received data */
                        uint32_t overlap = conn->recv_next - seq_start;
                        trim_start = conn->recv_next;
                        trim_data = data + overlap;
                        trim_len = data_len - overlap;
                    }

                    /* Insert into reassembly queue */
                    int result = reasm_queue_insert(conn->reasm_queue, trim_start,
                                                   trim_start + trim_len, trim_data,
                                                   trim_len);

                    if (result == 0) {
                        /* Successfully buffered - generate SACK blocks */
                        if (conn->sack_permitted) {
                            tcp_generate_sack_blocks(conn);
                        }

                        /* Send duplicate ACK with SACK info */
                        tcp_send_segment(conn, TCP_FLAG_ACK, NULL, 0);
                        conn->duplicate_acks++;
                    } else {
                        /* Buffer full or allocation failed - drop segment */
                        /* Sender will retransmit based on timeout */
                    }
                }
            }

        } else {
            /* seq_start < recv_next: Old duplicate or retransmission */
            if (SEQ_GT(seq_end, conn->recv_next)) {
                /* Partial retransmission with new data */
                uint32_t overlap = conn->recv_next - seq_start;
                if (overlap < data_len) {
                    /* Process new portion */
                    const uint8_t* new_data = data + overlap;
                    size_t new_len = data_len - overlap;

                    struct tcp_segment* segment = (struct tcp_segment*)kmalloc(
                        sizeof(struct tcp_segment));
                    if (segment) {
                        memcpy(&segment->header, header, sizeof(struct tcp_header));
                        segment->data = (uint8_t*)kmalloc(new_len);
                        if (segment->data) {
                            memcpy(segment->data, new_data, new_len);
                            segment->data_len = new_len;
                            segment->next = conn->recv_queue;
                            conn->recv_queue = segment;
                            conn->recv_next += new_len;
                            conn->recv_buffer_used += new_len;
                            conn->bytes_received += new_len;
                        } else {
                            kfree(segment);
                        }
                    }
                }
            }

            /* Send duplicate ACK (already received this data) */
            tcp_send_segment(conn, TCP_FLAG_ACK, NULL, 0);
            conn->duplicate_acks++;
        }
    }
    
    /* Handle state transitions */
    tcp_handle_state_transition(conn, header);
    
    conn->segments_received++;
    
    return 0;
}

/* Send data */
int tcp_send(struct tcp_connection* conn, const void* data, size_t len) {
    if (!conn || !data || len == 0) {
        return -1;
    }
    
    if (conn->state != TCP_ESTABLISHED) {
        return -1;
    }
    
    /* Check send window */
    uint32_t available = conn->send_window - conn->send_buffer_used;
    if (len > available) {
        len = available;
    }
    
    /* Check congestion window */
    if (len > conn->cwnd * TCP_DEFAULT_MSS) {
        len = conn->cwnd * TCP_DEFAULT_MSS;
    }
    
    /* Send data in segments */
    const uint8_t* data_ptr = (const uint8_t*)data;
    size_t sent = 0;
    
    while (sent < len) {
        size_t segment_len = len - sent;
        if (segment_len > TCP_DEFAULT_MSS) {
            segment_len = TCP_DEFAULT_MSS;
        }
        
        tcp_send_segment(conn, TCP_FLAG_ACK | TCP_FLAG_PSH, data_ptr + sent, segment_len);
        sent += segment_len;
    }
    
    return (int)sent;
}

/* Receive data */
int tcp_receive(struct tcp_connection* conn, void* buffer, size_t len) {
    if (!conn || !buffer || len == 0) {
        return -1;
    }
    
    if (conn->state != TCP_ESTABLISHED && conn->state != TCP_CLOSE_WAIT) {
        return -1;
    }
    
    /* Get data from receive queue */
    struct tcp_segment* segment = conn->recv_queue;
    if (!segment) {
        return 0;  /* No data available */
    }
    
    size_t copied = 0;
    uint8_t* buf = (uint8_t*)buffer;
    
    while (segment && copied < len) {
        size_t to_copy = segment->data_len;
        if (to_copy > len - copied) {
            to_copy = len - copied;
        }
        
        memcpy(buf + copied, segment->data, to_copy);
        copied += to_copy;
        
        if (to_copy == segment->data_len) {
            /* Segment fully consumed */
            struct tcp_segment* next = segment->next;
            if (segment->data) {
                kfree(segment->data);
            }
            kfree(segment);
            conn->recv_queue = next;
            conn->recv_buffer_used -= segment->data_len;
        } else {
            /* Partial consumption */
            memmove(segment->data, segment->data + to_copy, segment->data_len - to_copy);
            segment->data_len -= to_copy;
            conn->recv_buffer_used -= to_copy;
        }
        
        segment = conn->recv_queue;
    }
    
    return (int)copied;
}

/* Create TCP connection */
struct tcp_connection* tcp_create_connection(uint32_t local_addr, uint16_t local_port,
                                            uint32_t remote_addr, uint16_t remote_port) {
    struct tcp_connection* conn = (struct tcp_connection*)kmalloc(sizeof(struct tcp_connection));
    if (!conn) {
        return NULL;
    }
    
    memset(conn, 0, sizeof(struct tcp_connection));
    conn->local_addr = local_addr;
    conn->remote_addr = remote_addr;
    conn->local_port = local_port;
    conn->remote_port = remote_port;
    conn->state = TCP_CLOSED;
    
    /* Initialize sequence numbers */
    conn->send_seq = 0;  /* Would use random ISN */
    conn->send_una = conn->send_seq;
    conn->send_next = conn->send_seq;
    conn->recv_seq = 0;
    conn->recv_next = conn->recv_seq;
    
    /* Initialize windows */
    conn->recv_window = TCP_DEFAULT_WINDOW;
    conn->send_window = TCP_DEFAULT_WINDOW;
    
    /* Initialize congestion control */
    conn->cwnd = TCP_INITIAL_CWND;
    conn->ssthresh = TCP_INITIAL_SSTHRESH;
    conn->rtt = 100;  /* Initial RTT estimate */
    conn->rtt_var = 50;
    conn->retransmit_timeout = TCP_RTO_MIN;
    conn->in_slow_start = true;
    conn->in_congestion_avoidance = false;
    
    /* Initialize buffers */
    conn->send_buffer_size = 65535;
    conn->recv_buffer_size = 65535;

    /* Initialize reassembly queue */
    conn->reasm_queue = reasm_queue_create(TCP_REASM_QUEUE_MAX_BYTES);
    if (!conn->reasm_queue) {
        kfree(conn);
        return NULL;
    }

    /* Initialize SACK support */
    conn->sack_permitted = true;  /* Enable SACK by default */
    conn->sack_block_count = 0;

    /* Add to connection list */
    conn->next = connection_list;
    connection_list = conn;

    return conn;
}

/* Destroy TCP connection */
void tcp_destroy_connection(struct tcp_connection* conn) {
    if (!conn) {
        return;
    }

    /* Free send queue */
    struct tcp_segment* seg = conn->send_queue;
    while (seg) {
        struct tcp_segment* next = seg->next;
        if (seg->data) {
            kfree(seg->data);
        }
        kfree(seg);
        seg = next;
    }

    /* Free send unacked queue */
    seg = conn->send_unacked;
    while (seg) {
        struct tcp_segment* next = seg->next;
        if (seg->data) {
            kfree(seg->data);
        }
        kfree(seg);
        seg = next;
    }

    /* Free receive queue */
    seg = conn->recv_queue;
    while (seg) {
        struct tcp_segment* next = seg->next;
        if (seg->data) {
            kfree(seg->data);
        }
        kfree(seg);
        seg = next;
    }

    /* Destroy reassembly queue */
    if (conn->reasm_queue) {
        reasm_queue_destroy(conn->reasm_queue);
    }

    /* Remove from connection list */
    if (conn == connection_list) {
        connection_list = conn->next;
    } else {
        struct tcp_connection* prev = connection_list;
        while (prev && prev->next != conn) {
            prev = prev->next;
        }
        if (prev) {
            prev->next = conn->next;
        }
    }

    kfree(conn);
}

/* Find connection */
struct tcp_connection* tcp_find_connection(uint32_t local_addr, uint16_t local_port,
                                          uint32_t remote_addr, uint16_t remote_port) {
    struct tcp_connection* conn = connection_list;
    
    while (conn) {
        if (conn->local_addr == local_addr &&
            conn->local_port == local_port &&
            conn->remote_addr == remote_addr &&
            conn->remote_port == remote_port) {
            return conn;
        }
        conn = conn->next;
    }
    
    return NULL;
}

/* Initialize TCP */
int tcp_init(void) {
    connection_list = NULL;
    next_local_port = 32768;
    return 0;
}
