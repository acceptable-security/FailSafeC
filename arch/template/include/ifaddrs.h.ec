<%# /* -*- c -*- */
#include <net/if.h>
#include <ifaddrs.h>
#%>
/**
 * @file include/ifaddrs.h
 */
#ifndef __IFADDRS_H
#define __IFADDRS_H

#include <sys/socket.h>

struct __fsc_attribute__((named "stdlib_ifaddrs")) ifaddrs {
  struct ifaddrs *ifa_next;
  char *ifa_name;
  unsigned int ifa_flags;
  struct sockaddr *ifa_addr;
  struct sockaddr *ifa_netmask;
  struct sockaddr *ifa_dstaddr;
  void *ifa_data;
};

extern int getifaddrs(struct ifaddrs **);
extern void freeifaddrs(struct ifaddrs *);

#define IFF_UP          <%=d IFF_UP          %>
#define IFF_LOOPBACK    <%=d IFF_LOOPBACK    %>
#define IFF_POINTOPOINT <%=d IFF_POINTOPOINT %>

#endif
