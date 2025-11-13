/* AstraLisp OS I/O Port Functions */

#ifndef IO_H
#define IO_H

#include <stdint.h>

/* Port I/O functions */
static inline uint8_t inb(uint16_t port) {
    uint8_t ret;
    __asm__ volatile ("lbz %0, 0(%1)" : "=r"(ret) : "r"(port));
    return ret;
}

static inline void outb(uint16_t port, uint8_t value) {
    __asm__ volatile ("stb %0, 0(%1)" : : "r"(value), "r"(port));
}

static inline uint16_t inw(uint16_t port) {
    uint16_t ret;
    __asm__ volatile ("lhz %0, 0(%1)" : "=r"(ret) : "r"(port));
    return ret;
}

static inline void outw(uint16_t port, uint16_t value) {
    __asm__ volatile ("sth %0, 0(%1)" : : "r"(value), "r"(port));
}

static inline uint32_t inl(uint16_t port) {
    uint32_t ret;
    __asm__ volatile ("lwz %0, 0(%1)" : "=r"(ret) : "r"(port));
    return ret;
}

static inline void outl(uint16_t port, uint32_t value) {
    __asm__ volatile ("stw %0, 0(%1)" : : "r"(value), "r"(port));
}

/* Memory-mapped I/O */
static inline uint8_t mmio_read8(volatile void* addr) {
    return *(volatile uint8_t*)addr;
}

static inline void mmio_write8(volatile void* addr, uint8_t value) {
    *(volatile uint8_t*)addr = value;
}

static inline uint16_t mmio_read16(volatile void* addr) {
    return *(volatile uint16_t*)addr;
}

static inline void mmio_write16(volatile void* addr, uint16_t value) {
    *(volatile uint16_t*)addr = value;
}

static inline uint32_t mmio_read32(volatile void* addr) {
    return *(volatile uint32_t*)addr;
}

static inline void mmio_write32(volatile void* addr, uint32_t value) {
    *(volatile uint32_t*)addr = value;
}

static inline uint64_t mmio_read64(volatile void* addr) {
    return *(volatile uint64_t*)addr;
}

static inline void mmio_write64(volatile void* addr, uint64_t value) {
    *(volatile uint64_t*)addr = value;
}

#endif /* IO_H */
