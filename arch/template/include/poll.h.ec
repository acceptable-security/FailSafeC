<%#shared /* -*- c -*- */
#include <poll.h>
#%>
<%#cflags -D_XOPEN_SOURCE=500 #%>
/**
 * @file include/poll.h
 */
#ifndef __POLL_H
#define __POLL_H

struct __fsc_attribute__((named "stdlib_pollfd")) pollfd {
  int fd;
  short events;
  short revents;
};

typedef unsigned nfds_t;

extern int poll(struct pollfd *, nfds_t, int);

#define POLLIN     <%=d POLLIN     %>
#define POLLRDNORM <%=d POLLRDNORM %>
#define POLLRDBAND <%=d POLLRDBAND %>
#define POLLPRI    <%=d POLLPRI    %>
#define POLLOUT    <%=d POLLOUT    %>
#define POLLWRNORM <%=d POLLWRNORM %>
#define POLLWRBAND <%=d POLLWRBAND %>
#define POLLERR    <%=d POLLERR    %>
#define POLLHUP    <%=d POLLHUP    %>
#define POLLNVAL   <%=d POLLNVAL   %>

#endif
