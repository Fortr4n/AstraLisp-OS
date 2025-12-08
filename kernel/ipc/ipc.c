/* AstraLisp OS - Inter-Process Communication */

#include "ipc.h"
#include "../process/process.h"
#include "../mm/heap.h"
#include <string.h>

/* Send message (Blocking? No, strictly copying and non-blocking for sender usually, unless queue full) 
   We implemented unbounded queue for now, so non-blocking send.
*/
int ipc_send(uint32_t pid, const void* data, uint32_t len) {
    if (!data || len > 256) return -1;
    
    /* Find process */
    extern struct process* process_list; /* Need locking for process list? Yes.. assume global lock or safe traversal */
    /* Ideally we use process_get(pid) */
    
    struct process* target = process_list;
    while (target && target->pid != pid) {
        target = target->next;
    }
    
    if (!target) return -1; /* PID not found */
    
    /* Allocate message */
    struct message* msg = (struct message*)kmalloc(sizeof(struct message));
    if (!msg) return -1; // ENOMEM
    
    msg->sender_pid = 0; /* TODO: get current PID */
    struct process* current = process_get_current();
    if (current) msg->sender_pid = current->pid;
    
    msg->length = len;
    memcpy(msg->data, data, len);
    msg->next = NULL;
    
    /* Enqueue */
    mutex_acquire(&target->msg_lock);
    
    if (target->msg_queue_tail) {
        target->msg_queue_tail->next = msg;
        target->msg_queue_tail = msg;
    } else {
        target->msg_queue_head = msg;
        target->msg_queue_tail = msg;
    }
    
    mutex_release(&target->msg_lock);
    
    /* Wake up receiver */
    wake_up(&target->msg_wait);
    
    return 0;
}

/* Receive message (Blocking) */
int ipc_receive(void* buffer, uint32_t len, uint32_t* sender_pid) {
    if (!buffer) return -1;
    
    struct process* current = process_get_current();
    if (!current) return -1;
    
    /* Loop checks condition */
    for (;;) {
        mutex_acquire(&current->msg_lock);
        
        if (current->msg_queue_head) {
            /* Message available */
            struct message* msg = current->msg_queue_head;
            current->msg_queue_head = msg->next;
            if (!current->msg_queue_head) {
                current->msg_queue_tail = NULL;
            }
            mutex_release(&current->msg_lock);
            
            /* Copy out */
            uint32_t copy_len = len < msg->length ? len : msg->length;
            memcpy(buffer, msg->data, copy_len);
            if (sender_pid) *sender_pid = msg->sender_pid;
            
            kfree(msg);
            return copy_len;
        }
        
        mutex_release(&current->msg_lock);
        
        /* Sleep */
        wait_event(&current->msg_wait);
    }
}
