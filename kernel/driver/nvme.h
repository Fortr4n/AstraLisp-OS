/* AstraLisp OS - NVMe Driver Header */
/* NVMe 1.4 Compliant */

#ifndef _KERNEL_DRIVER_NVME_H
#define _KERNEL_DRIVER_NVME_H

#include <stdint.h>
#include <stdbool.h>

/* NVMe Register Offsets (BAR0) */
#define NVME_REG_CAP      0x0000  /* Controller Capabilities */
#define NVME_REG_VS       0x0008  /* Version */
#define NVME_REG_INTMS    0x000C  /* Interrupt Mask Set */
#define NVME_REG_INTMC    0x0010  /* Interrupt Mask Clear */
#define NVME_REG_CC       0x0014  /* Controller Configuration */
#define NVME_REG_CSTS     0x001C  /* Controller Status */
#define NVME_REG_NSSR     0x0020  /* NVM Subsystem Reset */
#define NVME_REG_AQA      0x0024  /* Admin Queue Attributes */
#define NVME_REG_ASQ      0x0028  /* Admin Submission Queue Base */
#define NVME_REG_ACQ      0x0030  /* Admin Completion Queue Base */

/* Controller Configuration (CC) */
#define NVME_CC_ENABLE      (1 << 0)
#define NVME_CC_CSS_NVM     (0 << 4)
#define NVME_CC_MPS_SHIFT   7
#define NVME_CC_IOSQES_SHIFT 16
#define NVME_CC_IOCQES_SHIFT 20

/* Controller Status (CSTS) */
#define NVME_CSTS_RDY       (1 << 0)
#define NVME_CSTS_CFS       (1 << 1)
#define NVME_CSTS_SHST_MASK (3 << 2)

/* Admin Command Opcodes */
#define NVME_ADMIN_DELETE_SQ    0x00
#define NVME_ADMIN_CREATE_SQ    0x01
#define NVME_ADMIN_DELETE_CQ    0x04
#define NVME_ADMIN_CREATE_CQ    0x05
#define NVME_ADMIN_IDENTIFY     0x06
#define NVME_ADMIN_SET_FEATURES 0x09
#define NVME_ADMIN_GET_FEATURES 0x0A

/* NVM Command Opcodes */
#define NVME_CMD_FLUSH          0x00
#define NVME_CMD_WRITE          0x01
#define NVME_CMD_READ           0x02

/* Feature Identifiers */
#define NVME_FEAT_NUM_QUEUES    0x07

/* Identify CNS Values */
#define NVME_IDENTIFY_NS        0x00
#define NVME_IDENTIFY_CTRL      0x01
#define NVME_IDENTIFY_NS_LIST   0x02

/* Queue Entry Sizes */
#define NVME_SQE_SIZE 64
#define NVME_CQE_SIZE 16
#define NVME_ADMIN_QUEUE_DEPTH 32
#define NVME_IO_QUEUE_DEPTH 128

/* Submission Queue Entry (64 bytes) */
struct nvme_sqe {
    uint8_t  opcode;
    uint8_t  flags;
    uint16_t cid;
    uint32_t nsid;
    uint32_t cdw2;
    uint32_t cdw3;
    uint64_t mptr;
    uint64_t prp1;
    uint64_t prp2;
    uint32_t cdw10;
    uint32_t cdw11;
    uint32_t cdw12;
    uint32_t cdw13;
    uint32_t cdw14;
    uint32_t cdw15;
} __attribute__((packed));

/* Completion Queue Entry (16 bytes) */
struct nvme_cqe {
    uint32_t result;
    uint32_t rsvd;
    uint16_t sq_head;
    uint16_t sq_id;
    uint16_t cid;
    uint16_t status;
} __attribute__((packed));

/* NVMe Queue */
struct nvme_queue {
    uint16_t qid;
    uint16_t depth;
    volatile struct nvme_sqe* sq;   /* Submission Queue */
    volatile struct nvme_cqe* cq;   /* Completion Queue */
    volatile uint32_t* sq_doorbell;
    volatile uint32_t* cq_doorbell;
    uint16_t sq_tail;
    uint16_t cq_head;
    uint8_t  cq_phase;
};

/* NVMe Namespace */
struct nvme_namespace {
    uint32_t nsid;
    uint64_t num_blocks;
    uint32_t block_size;
    uint8_t  lba_shift;
};

/* NVMe Controller */
struct nvme_controller {
    volatile uint32_t* regs;        /* BAR0 mapped registers */
    uint64_t cap;                   /* Capabilities */
    uint32_t version;
    uint16_t max_qsize;             /* Max queue size */
    uint8_t  doorbell_stride;       /* 2^n * 4 bytes */
    struct nvme_queue admin_queue;
    struct nvme_queue io_queue;     /* Single I/O queue for simplicity */
    struct nvme_namespace ns;       /* Primary namespace */
    uint16_t next_cid;
};

/* Initialize NVMe controller */
int nvme_init(struct nvme_controller* ctrl, volatile void* bar0);

/* Probe for NVMe devices on PCI */
int nvme_probe(void);

/* Read blocks */
int nvme_read(struct nvme_controller* ctrl, uint64_t lba, void* buffer, size_t blocks);

/* Write blocks */
int nvme_write(struct nvme_controller* ctrl, uint64_t lba, const void* buffer, size_t blocks);

/* Shutdown controller */
int nvme_shutdown(struct nvme_controller* ctrl);

#endif
