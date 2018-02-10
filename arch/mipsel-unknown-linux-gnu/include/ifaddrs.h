/* Generated file -- do not edit. */
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

#define IFF_UP          1
#define IFF_LOOPBACK    8
#define IFF_POINTOPOINT 16

#endif
