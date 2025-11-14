/* AstraLisp OS AHCI Driver Implementation */

#include "ahci.h"
#include "../hal/io.h"
#include "../hal/mmu.h"
#include "../mm/heap.h"
#include "../mm/pmm.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

#define AHCI_CMD_LIST_SIZE 1024
#define AHCI_FIS_SIZE 256
#define AHCI_PRDT_SIZE 16384
#define AHCI_CMD_TABLE_SIZE 128

/* Wait for port to be ready */
static int ahci_wait_ready(struct ahci_port* port, uint32_t mask, uint32_t value) {
    uint32_t timeout = 100000;
    
    while (timeout--) {
        uint32_t status = mmio_read32(port->base + AHCI_PxTFD);
        if ((status & mask) == value) {
            return 0;
        }
        /* Small delay */
        for (volatile int i = 0; i < 100; i++);
    }
    
    return -1;
}

/* Wait for command completion */
static int ahci_wait_command(struct ahci_port* port) {
    uint32_t timeout = 100000;
    
    while (timeout--) {
        uint32_t ci = mmio_read32(port->base + AHCI_PxCI);
        if ((ci & (1 << port->cmd_slot)) == 0) {
            return 0;
        }
        /* Small delay */
        for (volatile int i = 0; i < 100; i++);
    }
    
    return -1;
}

/* Initialize AHCI controller */
int ahci_init(struct ahci_controller* ctrl, volatile void* mmio_base) {
    if (!ctrl || !mmio_base) {
        return -1;
    }
    
    ctrl->mmio_base = (volatile uint32_t*)mmio_base;
    ctrl->capabilities = mmio_read32(ctrl->mmio_base + AHCI_CAP);
    ctrl->version = mmio_read32(ctrl->mmio_base + AHCI_VS);
    ctrl->implemented_ports = mmio_read32(ctrl->mmio_base + AHCI_PI);
    
    /* Count implemented ports */
    ctrl->port_count = 0;
    for (uint32_t i = 0; i < 32; i++) {
        if (ctrl->implemented_ports & (1 << i)) {
            ctrl->port_count++;
        }
    }
    
    /* Enable AHCI */
    uint32_t ghc = mmio_read32(ctrl->mmio_base + AHCI_GHC);
    if (!(ghc & (1 << 31))) {
        mmio_write32(ctrl->mmio_base + AHCI_GHC, ghc | (1 << 31));
        
        /* Wait for AHCI to be ready */
        uint32_t timeout = 100000;
        while (timeout--) {
            ghc = mmio_read32(ctrl->mmio_base + AHCI_GHC);
            if (ghc & (1 << 31)) {
                break;
            }
            for (volatile int i = 0; i < 100; i++);
        }
    }
    
    /* Initialize all implemented ports */
    for (uint32_t i = 0; i < 32; i++) {
        if (ctrl->implemented_ports & (1 << i)) {
            ahci_port_init(ctrl, i);
        }
    }
    
    return 0;
}

/* Initialize AHCI port */
int ahci_port_init(struct ahci_controller* ctrl, uint32_t port_num) {
    if (!ctrl || port_num >= 32) {
        return -1;
    }
    
    struct ahci_port* port = &ctrl->ports[port_num];
    port->port_number = port_num;
    port->base = ctrl->mmio_base + 0x100 + (port_num * 0x80);
    
    /* Check if port is implemented */
    if (!(ctrl->implemented_ports & (1 << port_num))) {
        return -1;
    }
    
    /* Stop port */
    uint32_t cmd = mmio_read32(port->base + AHCI_PxCMD);
    if (cmd & (1 << 0)) {
        mmio_write32(port->base + AHCI_PxCMD, cmd & ~(1 << 0));
        ahci_wait_ready(port, (1 << 15), 0);
    }
    
    /* Allocate command list */
    port->cmd_list = (struct ahci_cmd_header*)pmm_alloc();
    if (!port->cmd_list) {
        return -1;
    }
    memset(port->cmd_list, 0, AHCI_CMD_LIST_SIZE);
    
    uint64_t cmd_list_phys = virt_to_phys((uintptr_t)port->cmd_list);
    mmio_write32(port->base + AHCI_PxCLB, (uint32_t)cmd_list_phys);
    mmio_write32(port->base + AHCI_PxCLBU, (uint32_t)(cmd_list_phys >> 32));
    
    /* Allocate FIS */
    port->fis = (struct ahci_fis*)pmm_alloc();
    if (!port->fis) {
        pmm_free(port->cmd_list);
        return -1;
    }
    memset(port->fis, 0, AHCI_FIS_SIZE);
    
    uint64_t fis_phys = virt_to_phys((uintptr_t)port->fis);
    mmio_write32(port->base + AHCI_PxFB, (uint32_t)fis_phys);
    mmio_write32(port->base + AHCI_PxFBU, (uint32_t)(fis_phys >> 32));
    
    /* Allocate PRDT for command slot 0 */
    port->prdt = (struct ahci_prdt_entry*)pmm_alloc();
    if (!port->prdt) {
        pmm_free(port->fis);
        pmm_free(port->cmd_list);
        return -1;
    }
    memset(port->prdt, 0, AHCI_PRDT_SIZE);
    
    /* Allocate command table */
    port->cmd_table = (uint32_t*)pmm_alloc();
    if (!port->cmd_table) {
        pmm_free(port->prdt);
        pmm_free(port->fis);
        pmm_free(port->cmd_list);
        return -1;
    }
    memset(port->cmd_table, 0, AHCI_CMD_TABLE_SIZE);
    
    uint64_t cmd_table_phys = virt_to_phys((uintptr_t)port->cmd_table);
    port->cmd_list[0].ctba = cmd_table_phys;
    port->cmd_list[0].ctbau = cmd_table_phys >> 32;
    
    port->cmd_slot = 0;
    port->max_slots = ((ctrl->capabilities >> 8) & 0x1F) + 1;
    
    /* Check port signature */
    port->sata_signature = mmio_read32(port->base + AHCI_PxSIG);
    port->sata_status = mmio_read32(port->base + AHCI_PxSSTS);
    
    /* Start port */
    return ahci_port_start(port);
}

/* Start port */
int ahci_port_start(struct ahci_port* port) {
    if (!port) {
        return -1;
    }
    
    /* Clear error status */
    mmio_write32(port->base + AHCI_PxSERR, 0xFFFFFFFF);
    
    /* Clear interrupt status */
    mmio_write32(port->base + AHCI_PxIS, 0xFFFFFFFF);
    
    /* Enable interrupts */
    mmio_write32(port->base + AHCI_PxIE, 0xFFFFFFFF);
    
    /* Start port */
    uint32_t cmd = mmio_read32(port->base + AHCI_PxCMD);
    cmd |= (1 << 0);  /* Start */
    cmd |= (1 << 4);  /* FIS receive enable */
    mmio_write32(port->base + AHCI_PxCMD, cmd);
    
    /* Wait for port to be ready */
    return ahci_wait_ready(port, (1 << 15), 0);
}

/* Stop port */
int ahci_port_stop(struct ahci_port* port) {
    if (!port) {
        return -1;
    }
    
    uint32_t cmd = mmio_read32(port->base + AHCI_PxCMD);
    cmd &= ~(1 << 0);  /* Stop */
    cmd &= ~(1 << 4);  /* FIS receive disable */
    mmio_write32(port->base + AHCI_PxCMD, cmd);
    
    return ahci_wait_ready(port, (1 << 15), 0);
}

/* Read from AHCI device */
int ahci_read(struct ahci_port* port, uint64_t lba, void* buffer, size_t count) {
    if (!port || !buffer || count == 0) {
        return -1;
    }
    
    /* Prepare command FIS */
    struct ahci_fis* fis = (struct ahci_fis*)port->cmd_table;
    memset(fis, 0, sizeof(struct ahci_fis));
    
    fis->type = 0x27;  /* Register FIS - Host to Device */
    fis->c = 1;        /* Command */
    fis->command = 0x25;  /* READ DMA EXT */
    fis->device = 0x40;   /* LBA mode */
    
    /* LBA address */
    fis->lba_low = (uint8_t)(lba & 0xFF);
    fis->lba_mid = (uint8_t)((lba >> 8) & 0xFF);
    fis->lba_high = (uint8_t)((lba >> 16) & 0xFF);
    fis->device |= (uint8_t)((lba >> 24) & 0x0F);
    fis->lba_low_ext = (uint8_t)((lba >> 28) & 0xFF);
    fis->lba_mid_ext = (uint8_t)((lba >> 36) & 0xFF);
    fis->lba_high_ext = (uint8_t)((lba >> 44) & 0xFF);
    
    /* Sector count */
    uint16_t sectors = (uint16_t)count;
    fis->count_low = (uint8_t)(sectors & 0xFF);
    fis->count_high = (uint8_t)((sectors >> 8) & 0xFF);
    
    /* Setup PRDT */
    uint64_t buffer_phys = virt_to_phys((uintptr_t)buffer);
    size_t transfer_size = count * 512;
    
    port->prdt[0].dba = buffer_phys;
    port->prdt[0].dbau = buffer_phys >> 32;
    port->prdt[0].dbc = (transfer_size - 1) & 0x3FFFFF;
    port->prdt[0].i = 1;  /* Interrupt on completion */
    
    /* Setup command header */
    port->cmd_list[0].flags = (5 << 0) | (1 << 16);  /* FIS length, write */
    port->cmd_list[0].prdtl = 1;  /* One PRDT entry */
    port->cmd_list[0].prdbc = 0;
    
    /* Issue command */
    mmio_write32(port->base + AHCI_PxCI, 1 << port->cmd_slot);
    
    /* Wait for completion */
    if (ahci_wait_command(port) != 0) {
        return -1;
    }
    
    /* Check for errors */
    uint32_t tfd = mmio_read32(port->base + AHCI_PxTFD);
    if (tfd & (1 << 0)) {  /* Error bit */
        return -1;
    }
    
    return (int)count;
}

/* Write to AHCI device */
int ahci_write(struct ahci_port* port, uint64_t lba, const void* buffer, size_t count) {
    if (!port || !buffer || count == 0) {
        return -1;
    }
    
    /* Prepare command FIS */
    struct ahci_fis* fis = (struct ahci_fis*)port->cmd_table;
    memset(fis, 0, sizeof(struct ahci_fis));
    
    fis->type = 0x27;  /* Register FIS - Host to Device */
    fis->c = 1;        /* Command */
    fis->command = 0x35;  /* WRITE DMA EXT */
    fis->device = 0x40;   /* LBA mode */
    
    /* LBA address */
    fis->lba_low = (uint8_t)(lba & 0xFF);
    fis->lba_mid = (uint8_t)((lba >> 8) & 0xFF);
    fis->lba_high = (uint8_t)((lba >> 16) & 0xFF);
    fis->device |= (uint8_t)((lba >> 24) & 0x0F);
    fis->lba_low_ext = (uint8_t)((lba >> 28) & 0xFF);
    fis->lba_mid_ext = (uint8_t)((lba >> 36) & 0xFF);
    fis->lba_high_ext = (uint8_t)((lba >> 44) & 0xFF);
    
    /* Sector count */
    uint16_t sectors = (uint16_t)count;
    fis->count_low = (uint8_t)(sectors & 0xFF);
    fis->count_high = (uint8_t)((sectors >> 8) & 0xFF);
    
    /* Setup PRDT */
    uint64_t buffer_phys = virt_to_phys((uintptr_t)buffer);
    size_t transfer_size = count * 512;
    
    port->prdt[0].dba = buffer_phys;
    port->prdt[0].dbau = buffer_phys >> 32;
    port->prdt[0].dbc = (transfer_size - 1) & 0x3FFFFF;
    port->prdt[0].i = 1;  /* Interrupt on completion */
    
    /* Setup command header */
    port->cmd_list[0].flags = (5 << 0) | (1 << 16) | (1 << 6);  /* FIS length, write, ATAPI */
    port->cmd_list[0].prdtl = 1;  /* One PRDT entry */
    port->cmd_list[0].prdbc = 0;
    
    /* Issue command */
    mmio_write32(port->base + AHCI_PxCI, 1 << port->cmd_slot);
    
    /* Wait for completion */
    if (ahci_wait_command(port) != 0) {
        return -1;
    }
    
    /* Check for errors */
    uint32_t tfd = mmio_read32(port->base + AHCI_PxTFD);
    if (tfd & (1 << 0)) {  /* Error bit */
        return -1;
    }
    
    return (int)count;
}
