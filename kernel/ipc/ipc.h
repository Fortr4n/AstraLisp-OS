/* AstraLisp OS - IPC Header */

#ifndef IPC_H
#define IPC_H

#include <stdint.h>

/* Send message to PID */
int ipc_send(uint32_t pid, const void* data, uint32_t len);

/* Receive message (blocking) */
int ipc_receive(void* buffer, uint32_t len, uint32_t* sender_pid);

#endif
