<%# /* -*- c -*- */
#include <stdlib.h>
#include <string.h>

int find_ioctl_req(char *p)
{
#define IOCTL_ENTRY(req, name, is_ptr, tp, condition) \
  if (strcmp(#name, p) == 0) { \
    return req; \
  } else
#include "../../../../runtime/stdlib/ioctl_list.h"
  abort();
}

#%>
/**
 * @file include/sys/ioctl.h
 */
#ifndef __SYS_IOCTL_H
#define __SYS_IOCTL_H

extern int ioctl(int, int, ...);

#define TIOCGWINSZ <%=d find_ioctl_req("TIOCGWINSZ") %>
#define TIOCSWINSZ <%=d find_ioctl_req("TIOCSWINSZ") %>
#define FIONREAD   <%=d find_ioctl_req("FIONREAD")   %>

#endif
