/* AstraLisp OS Hardware Abstraction Layer Implementation */
/* Comprehensive device enumeration and power management for PowerPC */

#include "hal.h"
#include "../mm/heap.h"
#include "../drivers/opal/opal.h"
#include <stddef.h>
#include <string.h>

static struct device* device_list = NULL;
static uint32_t next_device_id = 1;

/* PCI Configuration Space Access */
#define PCI_CONFIG_ADDR 0xFEC00000
#define PCI_CONFIG_DATA 0xFEE00000

/* PCI Header Fields */
#define PCI_VENDOR_ID       0x00
#define PCI_DEVICE_ID       0x02
#define PCI_COMMAND         0x04
#define PCI_STATUS          0x06
#define PCI_CLASS_REVISION  0x08
#define PCI_HEADER_TYPE     0x0E
#define PCI_BAR0            0x10
#define PCI_INTERRUPT_LINE  0x3C

/* Read PCI config word */
static uint32_t pci_config_read(uint8_t bus, uint8_t slot, uint8_t func, uint8_t offset) {
    uint32_t address = (1 << 31) | ((uint32_t)bus << 16) | 
                       ((uint32_t)slot << 11) | ((uint32_t)func << 8) | 
                       (offset & 0xFC);
    
    volatile uint32_t* config_addr = (volatile uint32_t*)PCI_CONFIG_ADDR;
    volatile uint32_t* config_data = (volatile uint32_t*)PCI_CONFIG_DATA;
    
    *config_addr = address;
    return *config_data;
}

/* Write PCI config word */
static void pci_config_write(uint8_t bus, uint8_t slot, uint8_t func, uint8_t offset, uint32_t value) {
    uint32_t address = (1 << 31) | ((uint32_t)bus << 16) | 
                       ((uint32_t)slot << 11) | ((uint32_t)func << 8) | 
                       (offset & 0xFC);
    
    volatile uint32_t* config_addr = (volatile uint32_t*)PCI_CONFIG_ADDR;
    volatile uint32_t* config_data = (volatile uint32_t*)PCI_CONFIG_DATA;
    
    *config_addr = address;
    *config_data = value;
}

/* Create device from PCI function */
static struct device* pci_create_device(uint8_t bus, uint8_t slot, uint8_t func) {
    uint32_t vendor_device = pci_config_read(bus, slot, func, PCI_VENDOR_ID);
    uint16_t vendor_id = vendor_device & 0xFFFF;
    
    if (vendor_id == 0xFFFF || vendor_id == 0x0000) {
        return NULL;
    }
    
    struct device* dev = (struct device*)kmalloc(sizeof(struct device));
    if (!dev) {
        return NULL;
    }
    
    memset(dev, 0, sizeof(struct device));
    
    dev->id = next_device_id++;
    dev->type = DEVICE_TYPE_PCI;
    
    /* Read BAR0 */
    uint32_t bar0 = pci_config_read(bus, slot, func, PCI_BAR0);
    if (bar0 & 1) {
        /* I/O space */
        dev->base_address = (void*)(uintptr_t)(bar0 & ~0x3);
    } else {
        /* Memory space */
        dev->base_address = (void*)(uintptr_t)(bar0 & ~0xF);
    }
    
    /* Read IRQ */
    uint32_t irq_data = pci_config_read(bus, slot, func, PCI_INTERRUPT_LINE);
    dev->irq = irq_data & 0xFF;
    
    /* Enable bus mastering and memory/IO access */
    uint32_t command = pci_config_read(bus, slot, func, PCI_COMMAND);
    command |= 0x07; /* Memory + I/O + Bus Master */
    pci_config_write(bus, slot, func, PCI_COMMAND, command);
    
    return dev;
}

/* Initialize HAL */
int hal_init(void) {
    device_list = NULL;
    next_device_id = 1;
    return 0;
}

/* Enumerate devices */
int hal_enumerate_devices(void) {
    int devices_found = 0;
    
    /* Scan PCI buses */
    for (uint16_t bus = 0; bus < 256; bus++) {
        for (uint8_t slot = 0; slot < 32; slot++) {
            for (uint8_t func = 0; func < 8; func++) {
                struct device* dev = pci_create_device((uint8_t)bus, slot, func);
                if (dev) {
                    hal_register_device(dev);
                    devices_found++;
                }
                
                /* Check if multifunction device */
                if (func == 0) {
                    uint32_t header = pci_config_read((uint8_t)bus, slot, 0, PCI_HEADER_TYPE);
                    if ((header & 0x80) == 0) {
                        break; /* Not multifunction */
                    }
                }
            }
        }
    }
    
    return devices_found;
}

/* Register device */
int hal_register_device(struct device* dev) {
    if (!dev) {
        return -1;
    }
    
    dev->next = device_list;
    device_list = dev;
    
    return 0;
}

/* Find device by type */
struct device* hal_find_device(uint32_t type) {
    struct device* dev = device_list;
    
    while (dev) {
        if (dev->type == type) {
            return dev;
        }
        dev = dev->next;
    }
    
    return NULL;
}

/* Find device by ID */
struct device* hal_find_device_by_id(uint32_t id) {
    struct device* dev = device_list;
    
    while (dev) {
        if (dev->id == id) {
            return dev;
        }
        dev = dev->next;
    }
    
    return NULL;
}

/* Get all devices (returns head of list) */
struct device* hal_get_device_list(void) {
    return device_list;
}

/* Power management - suspend */
int hal_suspend(void) {
    /* Notify all devices of suspend */
    struct device* dev = device_list;
    while (dev) {
        if (dev->suspend) {
            dev->suspend(dev);
        }
        dev = dev->next;
    }
    
    /* Use OPAL to prepare for suspend */
    /* Note: POWER systems use different suspend mechanism */
    return 0;
}

/* Power management - resume */
int hal_resume(void) {
    /* Notify all devices of resume */
    struct device* dev = device_list;
    while (dev) {
        if (dev->resume) {
            dev->resume(dev);
        }
        dev = dev->next;
    }
    
    return 0;
}


/* Shutdown system */
int hal_shutdown(void) {
    opal_cec_power_down(0);
    return 0;
}

/* Reboot system */
int hal_reboot(void) {
    opal_cec_reboot();
    return 0;
}
/* Helper to convert BCD to binary */
static uint32_t bcd2bin(uint32_t val) {
    return ((val) & 0x0f) + ((val) >> 4) * 10;
}

/* Helper: Days in month */
static int days_in_month[] = {0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31};

/* Check if leap year */
static int is_leap(int year) {
    return (year % 4 == 0 && (year % 100 != 0 || year % 400 == 0));
}

/* Get RTC time in nanoseconds */
uint64_t hal_get_rtc_time(void) {
    uint32_t ymd;
    uint64_t hmsm;
    
    /* OPAL_RTC_READ = 3 */
    int rc = opal_rtc_read(&ymd, &hmsm);
    if (rc != 0) { /* OPAL_SUCCESS = 0 */
        return 0;
    }
    
    /* Decode OPAL format */
    /* YMD: 000Y YYYY MMMM DDDD */
    /* HMSM: HHHH MMMM SSSS MMMM MMMM MMMM MMMM */
    
    uint32_t year = bcd2bin((ymd >> 12) & 0xFFF) + 2000; /* Assume 2000+ */
    uint32_t month = bcd2bin((ymd >> 8) & 0xF);
    uint32_t day = bcd2bin(ymd & 0xFF);
    
    uint32_t hour = bcd2bin((hmsm >> 56) & 0xFF);
    uint32_t minute = bcd2bin((hmsm >> 48) & 0xFF);
    uint32_t second = bcd2bin((hmsm >> 40) & 0xFF);
    /* Milliseconds ignored for now as they are high precision */
    
    /* Convert to unix timestamp (seconds since 1970) */
    uint64_t days = 0;
    for (uint32_t y = 1970; y < year; y++) {
        days += is_leap(y) ? 366 : 365;
    }
    
    for (uint32_t m = 1; m < month; m++) {
        if (m == 2 && is_leap(year)) {
            days += 29;
        } else {
            days += days_in_month[m];
        }
    }
    
    days += day - 1;
    
    uint64_t total_seconds = days * 86400 + hour * 3600 + minute * 60 + second;
    return total_seconds * 1000000000ULL;
}
