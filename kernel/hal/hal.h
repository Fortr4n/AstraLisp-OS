/* AstraLisp OS Hardware Abstraction Layer */

#ifndef HAL_H
#define HAL_H

#include <stdint.h>
#include <stdbool.h>

/* Device structure */
struct device {
    uint32_t id;
    uint32_t type;
    void* base_address;
    uint32_t irq;
    struct device* next;
};

/* Device types */
#define DEVICE_TYPE_PCI 1
#define DEVICE_TYPE_ISA 2
#define DEVICE_TYPE_ACPI 3
#define DEVICE_TYPE_DT 4  /* Device tree */

/* Initialize HAL */
int hal_init(void);

/* Enumerate devices */
int hal_enumerate_devices(void);

/* Register device */
int hal_register_device(struct device* dev);

/* Find device by type */
struct device* hal_find_device(uint32_t type);

/* Power management */
int hal_suspend(void);
int hal_resume(void);

#endif /* HAL_H */
