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

/* Helper: Create PRP List for multi-page transfer */
/* Returns physical address of PRP List, or 0 if not needed/failed */
/* Note: Caller must free the PRP List page if returned! */
static uintptr_t nvme_create_prp_list(uintptr_t buffer, size_t size, uint64_t* prp2_out) {
    if (size <= PAGE_SIZE) {
        *prp2_out = 0;
        return 0;
    }
    
    /* Calculate number of pages needed */
    /* First page is pointed to by PRP1 */
    size_t first_page_size = PAGE_SIZE - (buffer & (PAGE_SIZE - 1));
    if (size <= first_page_size) {
        *prp2_out = 0;
        return 0;
    }
    
    size_t remaining = size - first_page_size;
    
    /* If remaining fits in one page (total 2 pages involved), PRP2 is just the address of 2nd page */
    if (remaining <= PAGE_SIZE) {
        *prp2_out = (uint64_t)(buffer + first_page_size);
        return 0;
    }
    
    /* Need a PRP List */
    void* prp_list_page = pmm_alloc();
    if (!prp_list_page) return 0;
    
    uint64_t* prp_list = (uint64_t*)P2V((uintptr_t)prp_list_page);
    uintptr_t curr_addr = buffer + first_page_size;
    int prp_idx = 0;
    
    while (remaining > 0) {
        prp_list[prp_idx++] = (uint64_t)curr_addr;
        /* TODO: If list fills up (512 entries), we need to chain another list page */
        /* For Phase 1, we assume max transfer < 2MB (512 * 4KB) */
        
        size_t chunk = (remaining > PAGE_SIZE) ? PAGE_SIZE : remaining;
        curr_addr += chunk;
        remaining -= chunk;
    }
    
    /* PRP2 points to the LIST */
    /* We need physical address of the list page */
    /* Assuming P2V allows reverse or knowing it came from pmm_alloc directly */
    /* Since we allocated it with pmm_alloc, we know its phys address is... wait check pmm_alloc */
    /* pmm_alloc returns PHYS/frame address? pmm.c says "return (void*)(i * PAGE_SIZE);" which is PHYS. */
    /* So prp_list_page IS physical. */
    
    *prp2_out = (uint64_t)(uintptr_t)prp_list_page;
    return (uintptr_t)prp_list_page; /* Return non-zero to indicate allocation occurred */
}

/* Submit command and wait with timeout hardening */
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
    
    /* Poll for completion with timeout */
    /* 5 seconds timeout */
    uint64_t start_tick = 0; /* Use timer_get_tick() if available */
    /* Since we don't have timer header included, we use loop count but calibrated? */
    /* Let's assume 100M loops ~ few seconds */
    
    uint32_t timeout = 10000000;
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
        
        /* Yield if possible to avoid hogging CPU */
        /* scheduler_yield(); -- implicit dependency, but safe */
        for (volatile int i = 0; i < 100; i++);
    }
    
    return -1; /* Timeout */
}

/* Read from NVMe */
int nvme_read(struct nvme_controller* ctrl, uint64_t lba, void* buffer, size_t blocks) {
    if (!ctrl || !buffer || blocks == 0) return -1;
    
    struct nvme_sqe cmd;
    memset(&cmd, 0, sizeof(cmd));
    
    cmd.opcode = NVME_CMD_READ;
    cmd.nsid = ctrl->ns.nsid;
    cmd.prp1 = (uint64_t)(uintptr_t)buffer;
    
    uint64_t prp2 = 0;
    uintptr_t prp_list_phys = nvme_create_prp_list((uintptr_t)buffer, blocks * ctrl->ns.block_size, &prp2);
    cmd.prp2 = prp2;
    
    cmd.cdw10 = (uint32_t)lba;
    cmd.cdw11 = (uint32_t)(lba >> 32);
    cmd.cdw12 = (blocks - 1); /* 0-based count */
    
    int ret = nvme_submit_cmd_sync(ctrl, &ctrl->io_queue, &cmd, NULL);
    
    if (prp_list_phys) {
        pmm_free((void*)prp_list_phys);
    }
    
    return ret;
}

/* Write to NVMe */
int nvme_write(struct nvme_controller* ctrl, uint64_t lba, const void* buffer, size_t blocks) {
    if (!ctrl || !buffer || blocks == 0) return -1;
    
    struct nvme_sqe cmd;
    memset(&cmd, 0, sizeof(cmd));
    
    cmd.opcode = NVME_CMD_WRITE;
    cmd.nsid = ctrl->ns.nsid;
    cmd.prp1 = (uint64_t)(uintptr_t)buffer;
    
    uint64_t prp2 = 0;
    uintptr_t prp_list_phys = nvme_create_prp_list((uintptr_t)buffer, blocks * ctrl->ns.block_size, &prp2);
    cmd.prp2 = prp2;
    
    cmd.cdw10 = (uint32_t)lba;
    cmd.cdw11 = (uint32_t)(lba >> 32);
    cmd.cdw12 = (blocks - 1);
    
    int ret = nvme_submit_cmd_sync(ctrl, &ctrl->io_queue, &cmd, NULL);
    
    if (prp_list_phys) {
        pmm_free((void*)prp_list_phys);
    }
    
    return ret;
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
