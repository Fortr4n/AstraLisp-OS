/* AstraLisp OS AHCI Driver */

#ifndef AHCI_H
#define AHCI_H

#include <stdint.h>
#include <stddef.h>
#include "storage.h"

/* AHCI register offsets */
#define AHCI_CAP 0x00
#define AHCI_GHC 0x04
#define AHCI_IS 0x08
#define AHCI_PI 0x0C
#define AHCI_VS 0x10
#define AHCI_CCCC 0x14
#define AHCI_CCCS 0x18
#define AHCI_EMLOC 0x1C
#define AHCI_EMCTL 0x20
#define AHCI_CAP2 0x24
#define AHCI_BOHC 0x28

/* Port registers (offset from port base) */
#define AHCI_PxCLB 0x00
#define AHCI_PxCLBU 0x04
#define AHCI_PxFB 0x08
#define AHCI_PxFBU 0x0C
#define AHCI_PxIS 0x10
#define AHCI_PxIE 0x14
#define AHCI_PxCMD 0x18
#define AHCI_PxTFD 0x20
#define AHCI_PxSIG 0x24
#define AHCI_PxSSTS 0x28
#define AHCI_PxSCTL 0x2C
#define AHCI_PxSERR 0x30
#define AHCI_PxSACT 0x34
#define AHCI_PxCI 0x38
#define AHCI_PxSNTF 0x3C
#define AHCI_PxFBS 0x40
#define AHCI_PxDEVSLP 0x44
#define AHCI_PxVS 0x70

/* Command list entry */
struct ahci_cmd_header {
    uint16_t flags;
    uint16_t prdtl;
    uint32_t prdbc;
    uint64_t ctba;
    uint64_t ctbau;
    uint32_t reserved[4];
};

/* Physical region descriptor table entry */
struct ahci_prdt_entry {
    uint64_t dba;
    uint64_t dbau;
    uint32_t reserved;
    uint32_t dbc:22;
    uint32_t reserved2:9;
    uint32_t i:1;
};

/* FIS structure */
struct ahci_fis {
    uint8_t type;
    uint8_t pmport:4;
    uint8_t reserved:3;
    uint8_t c:1;
    uint8_t command;
    uint8_t feature_low;
    uint8_t lba_low;
    uint8_t lba_mid;
    uint8_t lba_high;
    uint8_t device;
    uint8_t lba_low_ext;
    uint8_t lba_mid_ext;
    uint8_t lba_high_ext;
    uint8_t feature_high;
    uint8_t count_low;
    uint8_t count_high;
    uint8_t icc;
    uint8_t control;
    uint32_t reserved[2];
};

/* AHCI port structure */
struct ahci_port {
    uint32_t port_number;
    volatile uint32_t* base;
    struct ahci_cmd_header* cmd_list;
    struct ahci_fis* fis;
    struct ahci_prdt_entry* prdt;
    uint32_t* cmd_table;
    uint32_t cmd_slot;
    uint32_t max_slots;
    uint32_t sata_signature;
    uint32_t sata_status;
    struct block_device* block_dev;
};

/* AHCI controller structure */
struct ahci_controller {
    volatile uint32_t* mmio_base;
    uint32_t port_count;
    uint32_t implemented_ports;
    struct ahci_port ports[32];
    uint32_t capabilities;
    uint32_t version;
};

/* Initialize AHCI controller */
int ahci_init(struct ahci_controller* ctrl, volatile void* mmio_base);

/* Initialize AHCI port */
int ahci_port_init(struct ahci_controller* ctrl, uint32_t port_num);

/* Read from AHCI device */
int ahci_read(struct ahci_port* port, uint64_t lba, void* buffer, size_t count);

/* Write to AHCI device */
int ahci_write(struct ahci_port* port, uint64_t lba, const void* buffer, size_t count);

/* Start port */
int ahci_port_start(struct ahci_port* port);

/* Stop port */
int ahci_port_stop(struct ahci_port* port);

#endif /* AHCI_H */
