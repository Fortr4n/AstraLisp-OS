/* AstraLisp OS - NVMe Driver Implementation */
/* NVMe 1.4 Compliant for PowerPC */

#include "nvme.h"
#include "storage.h"
#include "../mm/pmm.h"
#include "../mm/heap.h"
#include "../hal/io.h"
#include <stddef.h>
#include <string.h>

/* Timeout values */
#define NVME_TIMEOUT_MS 5000

/* Helper: Read 32-bit register */
static inline uint32_t nvme_read32(struct nvme_controller* ctrl, uint32_t off) {
    return mmio_read32((volatile void*)((uintptr_t)ctrl->regs + off));
}

/* Helper: Write 32-bit register */
static inline void nvme_write32(struct nvme_controller* ctrl, uint32_t off, uint32_t val) {
    mmio_write32((volatile void*)((uintptr_t)ctrl->regs + off), val);
}

/* Helper: Read 64-bit register */
static inline uint64_t nvme_read64(struct nvme_controller* ctrl, uint32_t off) {
    uint64_t lo = mmio_read32((volatile void*)((uintptr_t)ctrl->regs + off));
    uint64_t hi = mmio_read32((volatile void*)((uintptr_t)ctrl->regs + off + 4));
    return lo | (hi << 32);
}

/* Helper: Write 64-bit register */
static inline void nvme_write64(struct nvme_controller* ctrl, uint32_t off, uint64_t val) {
    mmio_write32((volatile void*)((uintptr_t)ctrl->regs + off), (uint32_t)val);
    mmio_write32((volatile void*)((uintptr_t)ctrl->regs + off + 4), (uint32_t)(val >> 32));
}

/* Wait for controller ready */
static int nvme_wait_ready(struct nvme_controller* ctrl, bool ready) {
    uint32_t timeout = 500000;
    uint32_t expected = ready ? NVME_CSTS_RDY : 0;
    
    while (timeout--) {
        uint32_t csts = nvme_read32(ctrl, NVME_REG_CSTS);
        if ((csts & NVME_CSTS_RDY) == expected) {
            return 0;
        }
        /* Small delay */
        for (volatile int i = 0; i < 1000; i++);
    }
    
    return -1;
}

/* Allocate queue memory */
static int nvme_alloc_queue(struct nvme_queue* q, uint16_t qid, uint16_t depth, 
                            volatile uint32_t* sq_db, volatile uint32_t* cq_db) {
    size_t sq_size = depth * NVME_SQE_SIZE;
    size_t cq_size = depth * NVME_CQE_SIZE;
    
    /* Allocate physically contiguous memory */
    void* sq_phys = pmm_alloc_multiple((sq_size + 4095) / 4096);
    void* cq_phys = pmm_alloc_multiple((cq_size + 4095) / 4096);
    
    if (!sq_phys || !cq_phys) {
        if (sq_phys) pmm_free(sq_phys);
        if (cq_phys) pmm_free(cq_phys);
        return -1;
    }
    
    q->qid = qid;
    q->depth = depth;
    q->sq = (volatile struct nvme_sqe*)sq_phys;
    q->cq = (volatile struct nvme_cqe*)cq_phys;
    q->sq_doorbell = sq_db;
    q->cq_doorbell = cq_db;
    q->sq_tail = 0;
    q->cq_head = 0;
    q->cq_phase = 1;
    
    /* Clear queues */
    memset((void*)q->sq, 0, sq_size);
    memset((void*)q->cq, 0, cq_size);
    
    return 0;
}

/* Submit command and wait */
static int nvme_submit_cmd_sync(struct nvme_controller* ctrl, struct nvme_queue* q,
                                 struct nvme_sqe* cmd, uint32_t* result) {
    uint16_t cid = ctrl->next_cid++;
    uint16_t tail = q->sq_tail;
    
    /* Copy command to queue */
    cmd->cid = cid;
    memcpy((void*)&q->sq[tail], cmd, sizeof(struct nvme_sqe));
    
    /* Update tail and ring doorbell */
    q->sq_tail = (tail + 1) % q->depth;
    *q->sq_doorbell = q->sq_tail;
    
    /* Poll for completion */
    uint32_t timeout = 1000000;
    while (timeout--) {
        volatile struct nvme_cqe* cqe = &q->cq[q->cq_head];
        uint8_t phase = (cqe->status & 1);
        
        if (phase == q->cq_phase) {
            /* Check CID match */
            if (cqe->cid == cid) {
                /* Update head and ring doorbell */
                q->cq_head = (q->cq_head + 1) % q->depth;
                if (q->cq_head == 0) {
                    q->cq_phase ^= 1;
                }
                *q->cq_doorbell = q->cq_head;
                
                /* Check status */
                uint16_t status = cqe->status >> 1;
                if (result) *result = cqe->result;
                
                return (status == 0) ? 0 : -1;
            }
        }
        
        for (volatile int i = 0; i < 100; i++);
    }
    
    return -1; /* Timeout */
}

/* Identify controller */
static int nvme_identify_ctrl(struct nvme_controller* ctrl, void* buffer) {
    struct nvme_sqe cmd;
    memset(&cmd, 0, sizeof(cmd));
    
    cmd.opcode = NVME_ADMIN_IDENTIFY;
    cmd.prp1 = (uint64_t)(uintptr_t)buffer;
    cmd.cdw10 = NVME_IDENTIFY_CTRL;
    
    return nvme_submit_cmd_sync(ctrl, &ctrl->admin_queue, &cmd, NULL);
}

/* Identify namespace */
static int nvme_identify_ns(struct nvme_controller* ctrl, uint32_t nsid, void* buffer) {
    struct nvme_sqe cmd;
    memset(&cmd, 0, sizeof(cmd));
    
    cmd.opcode = NVME_ADMIN_IDENTIFY;
    cmd.nsid = nsid;
    cmd.prp1 = (uint64_t)(uintptr_t)buffer;
    cmd.cdw10 = NVME_IDENTIFY_NS;
    
    return nvme_submit_cmd_sync(ctrl, &ctrl->admin_queue, &cmd, NULL);
}

/* Set number of queues */
static int nvme_set_num_queues(struct nvme_controller* ctrl, uint16_t num, uint32_t* result) {
    struct nvme_sqe cmd;
    memset(&cmd, 0, sizeof(cmd));
    
    cmd.opcode = NVME_ADMIN_SET_FEATURES;
    cmd.cdw10 = NVME_FEAT_NUM_QUEUES;
    cmd.cdw11 = ((num - 1) << 16) | (num - 1);
    
    return nvme_submit_cmd_sync(ctrl, &ctrl->admin_queue, &cmd, result);
}

/* Create I/O completion queue */
static int nvme_create_io_cq(struct nvme_controller* ctrl, struct nvme_queue* q) {
    struct nvme_sqe cmd;
    memset(&cmd, 0, sizeof(cmd));
    
    cmd.opcode = NVME_ADMIN_CREATE_CQ;
    cmd.prp1 = (uint64_t)(uintptr_t)q->cq;
    cmd.cdw10 = ((q->depth - 1) << 16) | q->qid;
    cmd.cdw11 = 1; /* Physically contiguous */
    
    return nvme_submit_cmd_sync(ctrl, &ctrl->admin_queue, &cmd, NULL);
}

/* Create I/O submission queue */
static int nvme_create_io_sq(struct nvme_controller* ctrl, struct nvme_queue* q) {
    struct nvme_sqe cmd;
    memset(&cmd, 0, sizeof(cmd));
    
    cmd.opcode = NVME_ADMIN_CREATE_SQ;
    cmd.prp1 = (uint64_t)(uintptr_t)q->sq;
    cmd.cdw10 = ((q->depth - 1) << 16) | q->qid;
    cmd.cdw11 = (q->qid << 16) | 1; /* CQID and Physically contiguous */
    
    return nvme_submit_cmd_sync(ctrl, &ctrl->admin_queue, &cmd, NULL);
}

/* Initialize NVMe controller */
int nvme_init(struct nvme_controller* ctrl, volatile void* bar0) {
    if (!ctrl || !bar0) return -1;
    
    ctrl->regs = (volatile uint32_t*)bar0;
    ctrl->next_cid = 0;
    
    /* Read capabilities and version */
    ctrl->cap = nvme_read64(ctrl, NVME_REG_CAP);
    ctrl->version = nvme_read32(ctrl, NVME_REG_VS);
    
    /* Extract capabilities */
    ctrl->max_qsize = (ctrl->cap & 0xFFFF) + 1;
    ctrl->doorbell_stride = (ctrl->cap >> 32) & 0xF;
    
    /* Disable controller */
    nvme_write32(ctrl, NVME_REG_CC, 0);
    if (nvme_wait_ready(ctrl, false) != 0) {
        return -1;
    }
    
    /* Setup admin queue */
    uint32_t db_stride = 4 << ctrl->doorbell_stride;
    volatile uint32_t* admin_sq_db = (volatile uint32_t*)((uintptr_t)bar0 + 0x1000);
    volatile uint32_t* admin_cq_db = (volatile uint32_t*)((uintptr_t)bar0 + 0x1000 + db_stride);
    
    if (nvme_alloc_queue(&ctrl->admin_queue, 0, NVME_ADMIN_QUEUE_DEPTH, 
                         admin_sq_db, admin_cq_db) != 0) {
        return -1;
    }
    
    /* Write admin queue addresses */
    nvme_write64(ctrl, NVME_REG_ASQ, (uint64_t)(uintptr_t)ctrl->admin_queue.sq);
    nvme_write64(ctrl, NVME_REG_ACQ, (uint64_t)(uintptr_t)ctrl->admin_queue.cq);
    
    /* Write AQA (queue sizes) */
    uint32_t aqa = ((NVME_ADMIN_QUEUE_DEPTH - 1) << 16) | (NVME_ADMIN_QUEUE_DEPTH - 1);
    nvme_write32(ctrl, NVME_REG_AQA, aqa);
    
    /* Configure and enable controller */
    uint32_t cc = NVME_CC_ENABLE | NVME_CC_CSS_NVM;
    cc |= (0 << NVME_CC_MPS_SHIFT);     /* 4KB pages */
    cc |= (6 << NVME_CC_IOSQES_SHIFT);  /* 64-byte SQE */
    cc |= (4 << NVME_CC_IOCQES_SHIFT);  /* 16-byte CQE */
    nvme_write32(ctrl, NVME_REG_CC, cc);
    
    if (nvme_wait_ready(ctrl, true) != 0) {
        return -1;
    }
    
    /* Identify controller (allocate 4KB buffer) */
    void* id_buf = pmm_alloc();
    if (!id_buf) return -1;
    memset(id_buf, 0, 4096);
    
    if (nvme_identify_ctrl(ctrl, id_buf) != 0) {
        pmm_free(id_buf);
        return -1;
    }
    
    /* Get number of namespaces (offset 516, 4 bytes) */
    uint32_t nn = *((uint32_t*)((uintptr_t)id_buf + 516));
    pmm_free(id_buf);
    
    if (nn == 0) return -1;
    
    /* Set number of I/O queues */
    uint32_t queues_result;
    if (nvme_set_num_queues(ctrl, 1, &queues_result) != 0) {
        return -1;
    }
    
    /* Create I/O queue */
    volatile uint32_t* io_sq_db = (volatile uint32_t*)((uintptr_t)bar0 + 0x1000 + 2 * db_stride);
    volatile uint32_t* io_cq_db = (volatile uint32_t*)((uintptr_t)bar0 + 0x1000 + 3 * db_stride);
    
    if (nvme_alloc_queue(&ctrl->io_queue, 1, NVME_IO_QUEUE_DEPTH, io_sq_db, io_cq_db) != 0) {
        return -1;
    }
    
    if (nvme_create_io_cq(ctrl, &ctrl->io_queue) != 0) {
        return -1;
    }
    
    if (nvme_create_io_sq(ctrl, &ctrl->io_queue) != 0) {
        return -1;
    }
    
    /* Identify namespace 1 */
    id_buf = pmm_alloc();
    if (!id_buf) return -1;
    memset(id_buf, 0, 4096);
    
    if (nvme_identify_ns(ctrl, 1, id_buf) != 0) {
        pmm_free(id_buf);
        return -1;
    }
    
    /* Parse namespace info */
    ctrl->ns.nsid = 1;
    ctrl->ns.num_blocks = *((uint64_t*)((uintptr_t)id_buf + 0));  /* NSZE */
    uint8_t flbas = *((uint8_t*)((uintptr_t)id_buf + 26));        /* FLBAS */
    uint8_t lbaf_idx = flbas & 0xF;
    uint32_t lbaf = *((uint32_t*)((uintptr_t)id_buf + 128 + lbaf_idx * 4));
    ctrl->ns.lba_shift = (lbaf >> 16) & 0xFF;
    ctrl->ns.block_size = 1 << ctrl->ns.lba_shift;
    
    pmm_free(id_buf);
    
    return 0;
}

/* Read from NVMe */
int nvme_read(struct nvme_controller* ctrl, uint64_t lba, void* buffer, size_t blocks) {
    if (!ctrl || !buffer || blocks == 0) return -1;
    
    struct nvme_sqe cmd;
    memset(&cmd, 0, sizeof(cmd));
    
    cmd.opcode = NVME_CMD_READ;
    cmd.nsid = ctrl->ns.nsid;
    cmd.prp1 = (uint64_t)(uintptr_t)buffer;
    /* TODO: PRP2 for multi-page transfers */
    cmd.cdw10 = (uint32_t)lba;
    cmd.cdw11 = (uint32_t)(lba >> 32);
    cmd.cdw12 = (blocks - 1); /* 0-based count */
    
    return nvme_submit_cmd_sync(ctrl, &ctrl->io_queue, &cmd, NULL);
}

/* Write to NVMe */
int nvme_write(struct nvme_controller* ctrl, uint64_t lba, const void* buffer, size_t blocks) {
    if (!ctrl || !buffer || blocks == 0) return -1;
    
    struct nvme_sqe cmd;
    memset(&cmd, 0, sizeof(cmd));
    
    cmd.opcode = NVME_CMD_WRITE;
    cmd.nsid = ctrl->ns.nsid;
    cmd.prp1 = (uint64_t)(uintptr_t)buffer;
    cmd.cdw10 = (uint32_t)lba;
    cmd.cdw11 = (uint32_t)(lba >> 32);
    cmd.cdw12 = (blocks - 1);
    
    return nvme_submit_cmd_sync(ctrl, &ctrl->io_queue, &cmd, NULL);
}

/* Shutdown controller */
int nvme_shutdown(struct nvme_controller* ctrl) {
    if (!ctrl) return -1;
    
    /* Set shutdown notification (SHN = 01b) */
    uint32_t cc = nvme_read32(ctrl, NVME_REG_CC);
    cc |= (1 << 14); /* Normal shutdown */
    nvme_write32(ctrl, NVME_REG_CC, cc);
    
    /* Wait for shutdown complete */
    uint32_t timeout = 500000;
    while (timeout--) {
        uint32_t csts = nvme_read32(ctrl, NVME_REG_CSTS);
        if ((csts & NVME_CSTS_SHST_MASK) == (2 << 2)) { /* Shutdown complete */
            return 0;
        }
        for (volatile int i = 0; i < 1000; i++);
    }
    
    return -1;
}
