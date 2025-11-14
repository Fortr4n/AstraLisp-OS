/* AstraLisp OS LFSX Write-Ahead Logging */

#ifndef JOURNAL_H
#define JOURNAL_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/* Journal entry types */
#define JOURNAL_ENTRY_WRITE 1
#define JOURNAL_ENTRY_DELETE 2
#define JOURNAL_ENTRY_CREATE 3
#define JOURNAL_ENTRY_TRUNCATE 4
#define JOURNAL_ENTRY_COMMIT 5
#define JOURNAL_ENTRY_ABORT 6

/* Journal entry structure */
struct journal_entry {
    uint32_t type;
    uint32_t transaction_id;
    uint64_t block_number;
    uint32_t data_size;
    uint8_t* data;
    uint64_t checksum;
    uint64_t timestamp;
    struct journal_entry* next;
};

/* Journal structure */
struct journal {
    uint64_t journal_start_block;
    uint64_t journal_size_blocks;
    uint64_t current_block;
    uint32_t next_transaction_id;
    struct journal_entry* pending_entries;
    struct journal_entry* committed_entries;
    bool in_transaction;
    uint32_t current_transaction_id;
    int (*write_block)(uint64_t block, const void* data, size_t size);
    int (*read_block)(uint64_t block, void* data, size_t size);
};

/* Initialize journal */
int journal_init(struct journal* j, uint64_t start_block, uint64_t size_blocks,
                int (*write_block)(uint64_t, const void*, size_t),
                int (*read_block)(uint64_t, void*, size_t));

/* Begin transaction */
uint32_t journal_begin_transaction(struct journal* j);

/* Add write entry to transaction */
int journal_add_write(struct journal* j, uint32_t transaction_id, 
                      uint64_t block_number, const void* data, size_t size);

/* Add delete entry to transaction */
int journal_add_delete(struct journal* j, uint32_t transaction_id, uint64_t block_number);

/* Commit transaction */
int journal_commit_transaction(struct journal* j, uint32_t transaction_id);

/* Abort transaction */
int journal_abort_transaction(struct journal* j, uint32_t transaction_id);

/* Replay journal on mount */
int journal_replay(struct journal* j, 
                   int (*apply_write)(uint64_t block, const void* data, size_t size),
                   int (*apply_delete)(uint64_t block));

/* Checkpoint journal */
int journal_checkpoint(struct journal* j);

/* Get journal statistics */
void journal_get_stats(struct journal* j, uint64_t* entries_written, 
                       uint64_t* transactions_committed, uint64_t* transactions_aborted);

#endif /* JOURNAL_H */
