/**
 * @file include/arpa/inet.h
 */

#ifndef __ARPA_INET_H
#define __ARPA_INET_H

#include <netinet/in.h>
#include <stdint.h>

extern in_addr_t inet_addr(const char *);
extern char *inet_ntoa(struct in_addr);
extern const char *inet_ntop(int, const void *, char *, size_t);
extern int inet_pton(int, const char *, void *);

extern uint32_t htonl(uint32_t);
extern uint16_t htons(uint16_t);
extern uint32_t ntohl(uint32_t);
extern uint16_t ntohs(uint16_t);

#endif
