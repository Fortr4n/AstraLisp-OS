/* AstraLisp OS LFSX Write-Ahead Logging Implementation */

#include "journal.h"
#include "../../kernel/mm/heap.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

#define JOURNAL_HEADER_SIZE 512
#define JOURNAL_ENTRY_HEADER_SIZE 64
#define JOURNAL_BLOCK_SIZE 4096

/* Calculate checksum */
static uint64_t journal_calculate_checksum(const void* data, size_t size) {
    uint64_t checksum = 0;
    const uint8_t* bytes = (const uint8_t*)data;
    
    for (size_t i = 0; i < size; i++) {
        checksum = (checksum * 31) + bytes[i];
    }
    
    return checksum;
}

/* Write journal entry to disk */
static int journal_write_entry(struct journal* j, struct journal_entry* entry) {
    if (!j || !entry) {
        return -1;
    }
    
    /* Prepare entry buffer */
    size_t entry_size = JOURNAL_ENTRY_HEADER_SIZE + entry->data_size;
    size_t padded_size = (entry_size + JOURNAL_BLOCK_SIZE - 1) & ~(JOURNAL_BLOCK_SIZE - 1);
    uint8_t* buffer = (uint8_t*)kmalloc(padded_size);
    if (!buffer) {
        return -1;
    }
    
    memset(buffer, 0, padded_size);
    
    /* Write entry header */
    uint8_t* ptr = buffer;
    *((uint32_t*)ptr) = entry->type;
    ptr += 4;
    *((uint32_t*)ptr) = entry->transaction_id;
    ptr += 4;
    *((uint64_t*)ptr) = entry->block_number;
    ptr += 8;
    *((uint32_t*)ptr) = entry->data_size;
    ptr += 4;
    *((uint64_t*)ptr) = entry->timestamp;
    ptr += 8;
    *((uint64_t*)ptr) = 0;  /* Checksum placeholder */
    ptr += 8;
    
    /* Write data */
    if (entry->data && entry->data_size > 0) {
        memcpy(ptr, entry->data, entry->data_size);
    }
    
    /* Calculate and write checksum */
    uint64_t checksum = journal_calculate_checksum(buffer, entry_size);
    *((uint64_t*)(buffer + 40)) = checksum;
    entry->checksum = checksum;
    
    /* Write to journal */
    uint64_t blocks_to_write = padded_size / JOURNAL_BLOCK_SIZE;
    for (uint64_t i = 0; i < blocks_to_write; i++) {
        if (j->write_block(j->current_block + i, buffer + (i * JOURNAL_BLOCK_SIZE), 
                          JOURNAL_BLOCK_SIZE) != 0) {
            kfree(buffer);
            return -1;
        }
    }
    
    j->current_block += blocks_to_write;
    
    /* Wrap around if needed */
    if (j->current_block >= j->journal_start_block + j->journal_size_blocks) {
        j->current_block = j->journal_start_block;
    }
    
    kfree(buffer);
    return 0;
}

/* Initialize journal */
int journal_init(struct journal* j, uint64_t start_block, uint64_t size_blocks,
                int (*write_block)(uint64_t, const void*, size_t),
                int (*read_block)(uint64_t, void*, size_t)) {
    if (!j || !write_block || !read_block) {
        return -1;
    }
    
    j->journal_start_block = start_block;
    j->journal_size_blocks = size_blocks;
    j->current_block = start_block;
    j->next_transaction_id = 1;
    j->pending_entries = NULL;
    j->committed_entries = NULL;
    j->in_transaction = false;
    j->current_transaction_id = 0;
    j->write_block = write_block;
    j->read_block = read_block;
    
    return 0;
}

/* Begin transaction */
uint32_t journal_begin_transaction(struct journal* j) {
    if (!j || j->in_transaction) {
        return 0;
    }
    
    j->in_transaction = true;
    j->current_transaction_id = j->next_transaction_id++;
    j->pending_entries = NULL;
    
    return j->current_transaction_id;
}

/* Add write entry to transaction */
int journal_add_write(struct journal* j, uint32_t transaction_id, 
                      uint64_t block_number, const void* data, size_t size) {
    if (!j || !data || size == 0 || transaction_id != j->current_transaction_id) {
        return -1;
    }
    
    struct journal_entry* entry = (struct journal_entry*)kmalloc(sizeof(struct journal_entry));
    if (!entry) {
        return -1;
    }
    
    entry->type = JOURNAL_ENTRY_WRITE;
    entry->transaction_id = transaction_id;
    entry->block_number = block_number;
    entry->data_size = size;
    entry->data = (uint8_t*)kmalloc(size);
    if (!entry->data) {
        kfree(entry);
        return -1;
    }
    
    memcpy(entry->data, data, size);
    entry->checksum = journal_calculate_checksum(data, size);
    entry->timestamp = 0;  /* Would use real timestamp */
    entry->next = j->pending_entries;
    j->pending_entries = entry;
    
    return 0;
}

/* Add delete entry to transaction */
int journal_add_delete(struct journal* j, uint32_t transaction_id, uint64_t block_number) {
    if (!j || transaction_id != j->current_transaction_id) {
        return -1;
    }
    
    struct journal_entry* entry = (struct journal_entry*)kmalloc(sizeof(struct journal_entry));
    if (!entry) {
        return -1;
    }
    
    entry->type = JOURNAL_ENTRY_DELETE;
    entry->transaction_id = transaction_id;
    entry->block_number = block_number;
    entry->data_size = 0;
    entry->data = NULL;
    entry->checksum = 0;
    entry->timestamp = 0;
    entry->next = j->pending_entries;
    j->pending_entries = entry;
    
    return 0;
}

/* Commit transaction */
int journal_commit_transaction(struct journal* j, uint32_t transaction_id) {
    if (!j || transaction_id != j->current_transaction_id || !j->in_transaction) {
        return -1;
    }
    
    /* Write all pending entries to journal */
    struct journal_entry* entry = j->pending_entries;
    while (entry) {
        if (journal_write_entry(j, entry) != 0) {
            /* Rollback */
            j->in_transaction = false;
            return -1;
        }
        
        struct journal_entry* next = entry->next;
        entry->next = j->committed_entries;
        j->committed_entries = entry;
        entry = next;
    }
    
    /* Write commit marker */
    struct journal_entry commit_entry;
    commit_entry.type = JOURNAL_ENTRY_COMMIT;
    commit_entry.transaction_id = transaction_id;
    commit_entry.block_number = 0;
    commit_entry.data_size = 0;
    commit_entry.data = NULL;
    commit_entry.checksum = 0;
    commit_entry.timestamp = 0;
    commit_entry.next = NULL;
    
    if (journal_write_entry(j, &commit_entry) != 0) {
        j->in_transaction = false;
        return -1;
    }
    
    /* Flush journal */
    /* In real implementation, would sync to disk */
    
    j->pending_entries = NULL;
    j->in_transaction = false;
    
    return 0;
}

/* Abort transaction */
int journal_abort_transaction(struct journal* j, uint32_t transaction_id) {
    if (!j || transaction_id != j->current_transaction_id || !j->in_transaction) {
        return -1;
    }
    
    /* Write abort marker */
    struct journal_entry abort_entry;
    abort_entry.type = JOURNAL_ENTRY_ABORT;
    abort_entry.transaction_id = transaction_id;
    abort_entry.block_number = 0;
    abort_entry.data_size = 0;
    abort_entry.data = NULL;
    abort_entry.checksum = 0;
    abort_entry.timestamp = 0;
    abort_entry.next = NULL;
    
    journal_write_entry(j, &abort_entry);
    
    /* Free pending entries */
    struct journal_entry* entry = j->pending_entries;
    while (entry) {
        struct journal_entry* next = entry->next;
        if (entry->data) {
            kfree(entry->data);
        }
        kfree(entry);
        entry = next;
    }
    
    j->pending_entries = NULL;
    j->in_transaction = false;
    
    return 0;
}

/* Replay journal on mount */
int journal_replay(struct journal* j, 
                   int (*apply_write)(uint64_t block, const void* data, size_t size),
                   int (*apply_delete)(uint64_t block)) {
    if (!j || !apply_write || !apply_delete) {
        return -1;
    }
    
    /* Read journal from disk */
    uint64_t current = j->journal_start_block;
    uint32_t current_transaction = 0;
    bool in_transaction = false;
    
    while (current < j->journal_start_block + j->journal_size_blocks) {
        uint8_t buffer[JOURNAL_BLOCK_SIZE];
        if (j->read_block(current, buffer, JOURNAL_BLOCK_SIZE) != 0) {
            break;
        }
        
        /* Parse entry */
        uint32_t type = *((uint32_t*)buffer);
        uint32_t transaction_id = *((uint32_t*)(buffer + 4));
        uint64_t block_number = *((uint64_t*)(buffer + 8));
        uint32_t data_size = *((uint32_t*)(buffer + 16));
        uint64_t checksum = *((uint64_t*)(buffer + 40));
        
        /* Verify checksum */
        uint64_t calculated_checksum = journal_calculate_checksum(buffer, 
                                                                  JOURNAL_ENTRY_HEADER_SIZE + data_size);
        if (calculated_checksum != checksum) {
            /* Corrupted entry, stop replay */
            break;
        }
        
        if (type == JOURNAL_ENTRY_COMMIT) {
            if (in_transaction && transaction_id == current_transaction) {
                in_transaction = false;
            }
        } else if (type == JOURNAL_ENTRY_ABORT) {
            if (in_transaction && transaction_id == current_transaction) {
                in_transaction = false;
            }
        } else {
            if (!in_transaction) {
                current_transaction = transaction_id;
                in_transaction = true;
            }
            
            if (type == JOURNAL_ENTRY_WRITE && transaction_id == current_transaction) {
                void* data = buffer + JOURNAL_ENTRY_HEADER_SIZE;
                apply_write(block_number, data, data_size);
            } else if (type == JOURNAL_ENTRY_DELETE && transaction_id == current_transaction) {
                apply_delete(block_number);
            }
        }
        
        current++;
    }
    
    return 0;
}

/* Checkpoint journal */
int journal_checkpoint(struct journal* j) {
    if (!j) {
        return -1;
    }
    
    /* In a full implementation, this would:
     * 1. Apply all committed entries to main filesystem
     * 2. Clear journal
     * 3. Update superblock
     */
    
    j->current_block = j->journal_start_block;
    j->committed_entries = NULL;
    
    return 0;
}

/* Get journal statistics */
void journal_get_stats(struct journal* j, uint64_t* entries_written, 
                       uint64_t* transactions_committed, uint64_t* transactions_aborted) {
    if (!j) {
        return;
    }
    
    if (entries_written) {
        *entries_written = 0;
        struct journal_entry* entry = j->committed_entries;
        while (entry) {
            (*entries_written)++;
            entry = entry->next;
        }
    }
    
    if (transactions_committed) {
        *transactions_committed = j->next_transaction_id - 1;
    }
    
    if (transactions_aborted) {
        *transactions_aborted = 0;  /* Would track this */
    }
}
