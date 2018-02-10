/* Generated file -- do not edit. */
/**
 * @file include/netinet/in.h
 */
#ifndef __NETINET_IN_H
#define __NETINET_IN_H

#include <netinet/in_internal.h>
#include <sys/__types.h>

/* TODO inttypes.h */
#include <sys/socket.h>
#include <stdint.h>

typedef uint16_t in_port_t;
typedef uint32_t in_addr_t;

struct __fsc_attribute__((named "stdlib_in_addr")) in_addr {
  in_addr_t s_addr;
};

struct __fsc_attribute__((named "stdlib_sockaddr_in")) sockaddr_in {
  sa_family_t    sin_family;
  in_port_t      sin_port;
  struct in_addr sin_addr;
  char __dummy[8];
};

struct __fsc_attribute__((named "stdlib_in6_addr")) in6_addr {
  uint8_t s6_addr[16];
};

struct __fsc_attribute__((named "stdlib_sockaddr_in6")) sockaddr_in6 {
  sa_family_t sin6_family;
  in_port_t sin6_port;
  uint32_t sin6_flowinfo;
  struct in6_addr sin6_addr;
  uint32_t sin6_scope_id;
};

char *inet_ntoa(struct in_addr);

extern const struct in6_addr in6addr_any;
extern const struct in6_addr in6addr_loopback;

extern int __IN6_IS_ADDR_UNSPECIFIED(const struct in6_addr *);
extern int __IN6_IS_ADDR_LOOPBACK(const struct in6_addr *);
extern int __IN6_IS_ADDR_LINKLOCAL(const struct in6_addr *);
extern int __IN6_IS_ADDR_SITELOCAL(const struct in6_addr *);
extern int __IN6_IS_ADDR_V4MAPPED(const struct in6_addr *);
extern int __IN6_IS_ADDR_V4COMPAT(const struct in6_addr *);
extern int __IN6_IS_ADDR_MC_NODELOCAL(const struct in6_addr *);
extern int __IN6_IS_ADDR_MC_LINKLOCAL(const struct in6_addr *);
extern int __IN6_IS_ADDR_MC_SITELOCAL(const struct in6_addr *);
extern int __IN6_IS_ADDR_MC_ORGLOCAL(const struct in6_addr *);
extern int __IN6_IS_ADDR_MC_GLOBAL(const struct in6_addr *);

#define IN6_IS_ADDR_UNSPECIFIED(a)  __IN6_IS_ADDR_UNSPECIFIED(a)
#define IN6_IS_ADDR_LOOPBACK(a)     __IN6_IS_ADDR_LOOPBACK(a)
#define IN6_IS_ADDR_LINKLOCAL(a)    __IN6_IS_ADDR_LINKLOCAL(a)
#define IN6_IS_ADDR_SITELOCAL(a)    __IN6_IS_ADDR_SITELOCAL(a)
#define IN6_IS_ADDR_V4MAPPED(a)     __IN6_IS_ADDR_V4MAPPED(a)
#define IN6_IS_ADDR_V4COMPAT(a)     __IN6_IS_ADDR_V4COMPAT(a)
#define IN6_IS_ADDR_MC_NODELOCAL(a) __IN6_IS_ADDR_MC_NODELOCAL(a)
#define IN6_IS_ADDR_MC_LINKLOCAL(a) __IN6_IS_ADDR_MC_LINKLOCAL(a)
#define IN6_IS_ADDR_MC_SITELOCAL(a) __IN6_IS_ADDR_MC_SITELOCAL(a)
#define IN6_IS_ADDR_MC_ORGLOCAL(a)  __IN6_IS_ADDR_MC_ORGLOCAL(a)
#define IN6_IS_ADDR_MC_GLOBAL(a)    __IN6_IS_ADDR_MC_GLOBAL(a)

/* non standard */
extern int __IN_CLASSA(in_addr_t);
extern int __IN_CLASSB(in_addr_t);
extern int __IN_CLASSC(in_addr_t);
extern int __IN_CLASSD(in_addr_t);
extern int __IN_MULTICAST(in_addr_t);
extern int __IN_EXPERIMENTAL(in_addr_t);
extern int __IN_BADCLASS(in_addr_t);

#define IN_CLASSA(a) __IN_CLASSA(a)
#define IN_CLASSB(a) __IN_CLASSB(a)
#define IN_CLASSC(a) __IN_CLASSC(a)
#define IN_CLASSD(a) __IN_CLASSD(a)
#define IN_MULTICAST(a) __IN_MULTICAST(a)
#define IN_EXPERIMENTAL(a) __IN_EXPERIMENTAL(a)
#define IN_BADCLASS(a) __IN_BADCLASS(a)

#define IN6ADDR_ANY_INIT {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0}
#define IN6ADDR_LOOPBACK_INIT {0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1}

#endif
