<%# /* -*- c -*- */
#include <sys/un.h>
struct sockaddr_un un;
 #%>
/**
 * @file include/sys/un.h
 */
#ifndef __SYS_UN_H
#define __SYS_UN_H

#include <sys/socket.h>

struct sockaddr_un {
  sa_family_t sun_family;
  char sun_path[<%=d sizeof(un.sun_path) %>];
};

#endif
