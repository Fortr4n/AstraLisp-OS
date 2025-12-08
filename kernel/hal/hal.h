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

/* Find device by ID */
struct device* hal_find_device_by_id(uint32_t id);

/* Get device list */
struct device* hal_get_device_list(void);

/* Power management */
int hal_suspend(void);
int hal_resume(void);
int hal_shutdown(void);
int hal_reboot(void);

#endif /* HAL_H */

