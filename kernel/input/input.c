/* AstraLisp OS Input Subsystem - Implementation */

#include "input.h"
#include "../sync/spinlock.h"
#include "../sync/wait_queue.h"
#include <string.h>

#define INPUT_BUFFER_SIZE 256

/* Ring Buffer */
static input_event_t event_buffer[INPUT_BUFFER_SIZE];
static uint32_t head = 0; /* Write index */
static uint32_t tail = 0; /* Read index */
static spinlock_t input_lock;

/* Wait Queue for blocking readers */
static wait_queue_t input_wait_queue;

void input_init(void) {
    spinlock_init(&input_lock);
    wait_queue_init(&input_wait_queue);
    head = 0;
    tail = 0;
    memset(event_buffer, 0, sizeof(event_buffer));
}

int input_push_event(uint32_t type, uint32_t code, int32_t value) {
    spinlock_acquire(&input_lock);
    
    uint32_t next_head = (head + 1) % INPUT_BUFFER_SIZE;
    
    if (next_head == tail) {
        /* Buffer Full - Drop event */
        spinlock_release(&input_lock);
        return -1;
    }
    
    event_buffer[head].type = type;
    event_buffer[head].code = code;
    event_buffer[head].value = value;
    event_buffer[head].timestamp = 0; /* TODO: Get ticks */
    
    head = next_head;
    
    spinlock_release(&input_lock);
    
    /* Wake up waiting readers */
    wake_up(&input_wait_queue);
    
    return 0;
}

int input_pop_event(input_event_t* ev, bool blocking) {
    if (!ev) return 0;
    
    for (;;) {
        spinlock_acquire(&input_lock);
        
        if (head != tail) {
            /* Data available */
            *ev = event_buffer[tail];
            tail = (tail + 1) % INPUT_BUFFER_SIZE;
            
            spinlock_release(&input_lock);
            return 1;
        }
        
        spinlock_release(&input_lock);
        
        if (!blocking) {
            return 0;
        }
        
        /* Block waiting for data */
        wait_event(&input_wait_queue);
    }
}
