<%# /* -*- c -*- */
#include <ulimit.h>
#%>
/**
 * @file include/ulimit.h
 */
#ifndef __ULIMIT_H
#define __ULIMIT_H

#define UL_GETFSIZE <%=d UL_GETFSIZE %>
#define UL_SETFSIZE <%=d UL_SETFSIZE %>

extern long ulimit(int, ...);

#endif
