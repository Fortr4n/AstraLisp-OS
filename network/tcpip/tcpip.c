/* AstraLisp OS TCP/IP Stack Implementation */

#include "tcpip.h"
#include "tcp.h"
#include "ip.h"
#include "../../kernel/mm/heap.h"
#include <stddef.h>
#include <string.h>
#include <stdbool.h>

static uint32_t next_fd = 1;

/* Initialize TCP/IP stack */
int tcpip_init(void) {
    if (ip_init() != 0) {
        return -1;
    }
    
    if (tcp_init() != 0) {
        return -1;
    }
    
    return 0;
}

/* Create socket */
struct socket* socket_create(int domain, int type, int protocol) {
    struct socket* sock = (struct socket*)kmalloc(sizeof(struct socket));
    if (!sock) {
        return NULL;
    }
    
    memset(sock, 0, sizeof(struct socket));
    sock->fd = next_fd++;
    sock->local_port = 0;
    sock->remote_port = 0;
    sock->local_addr = 0;
    sock->remote_addr = 0;
    sock->state = 0;
    sock->tcp_conn = NULL;
    
    if (type == 1 && protocol == 6) {  /* SOCK_STREAM, TCP */
        sock->tcp_conn = tcp_create_connection(0, 0, 0, 0);
        if (!sock->tcp_conn) {
            kfree(sock);
            return NULL;
        }
    }
    
    return sock;
}

/* Bind socket */
int socket_bind(struct socket* sock, ip_addr_t addr, uint16_t port) {
    if (!sock) {
        return -1;
    }
    
    sock->local_addr = addr;
    sock->local_port = port;
    
    return 0;
}

/* Listen on socket */
int socket_listen(struct socket* sock, int backlog) {
    if (!sock) {
        return -1;
    }
    
    sock->state = 1;  /* LISTEN */
    
    return 0;
}

/* Accept connection */
struct socket* socket_accept(struct socket* sock) {
    if (!sock) {
        return NULL;
    }
    
    /* Placeholder */
    return NULL;
}

/* Connect socket */
int socket_connect(struct socket* sock, ip_addr_t addr, uint16_t port) {
    if (!sock || !sock->tcp_conn) {
        return -1;
    }
    
    struct tcp_connection* conn = (struct tcp_connection*)sock->tcp_conn;
    conn->remote_addr = addr;
    conn->remote_port = port;
    conn->state = TCP_SYN_SENT;
    
    /* Send SYN */
    /* This would call tcp_send_segment internally */
    sock->remote_addr = addr;
    sock->remote_port = port;
    sock->state = 2;  /* ESTABLISHED (simplified) */
    
    return 0;
}

/* Send data */
int socket_send(struct socket* sock, const void* data, size_t size) {
    if (!sock || !data || size == 0) {
        return -1;
    }
    
    if (sock->tcp_conn) {
        struct tcp_connection* conn = (struct tcp_connection*)sock->tcp_conn;
        return tcp_send(conn, data, size);
    }
    
    return -1;
}

/* Receive data */
int socket_recv(struct socket* sock, void* buffer, size_t size) {
    if (!sock || !buffer || size == 0) {
        return -1;
    }
    
    if (sock->tcp_conn) {
        struct tcp_connection* conn = (struct tcp_connection*)sock->tcp_conn;
        return tcp_receive(conn, buffer, size);
    }
    
    return -1;
}

/* Close socket */
void socket_close(struct socket* sock) {
    if (!sock) {
        return;
    }
    
    if (sock->tcp_conn) {
        struct tcp_connection* conn = (struct tcp_connection*)sock->tcp_conn;
        if (conn->state == TCP_ESTABLISHED) {
            /* Send FIN */
            conn->state = TCP_FIN_WAIT_1;
            /* This would call tcp_send_segment with FIN flag */
        }
        tcp_destroy_connection(conn);
    }
    
    kfree(sock);
}
