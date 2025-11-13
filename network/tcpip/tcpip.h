/* AstraLisp OS TCP/IP Stack */

#ifndef TCPIP_H
#define TCPIP_H

#include <stdint.h>
#include <stddef.h>

/* IP address */
typedef uint32_t ip_addr_t;

/* Socket structure */
struct socket {
    uint32_t fd;
    uint16_t local_port;
    uint16_t remote_port;
    ip_addr_t local_addr;
    ip_addr_t remote_addr;
    uint32_t state;
};

/* Initialize TCP/IP stack */
int tcpip_init(void);

/* Create socket */
struct socket* socket_create(int domain, int type, int protocol);

/* Bind socket */
int socket_bind(struct socket* sock, ip_addr_t addr, uint16_t port);

/* Listen on socket */
int socket_listen(struct socket* sock, int backlog);

/* Accept connection */
struct socket* socket_accept(struct socket* sock);

/* Connect socket */
int socket_connect(struct socket* sock, ip_addr_t addr, uint16_t port);

/* Send data */
int socket_send(struct socket* sock, const void* data, size_t size);

/* Receive data */
int socket_recv(struct socket* sock, void* buffer, size_t size);

/* Close socket */
void socket_close(struct socket* sock);

#endif /* TCPIP_H */
