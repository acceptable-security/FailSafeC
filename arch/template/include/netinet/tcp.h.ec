<%#
#include <netinet/tcp.h>
#%>
/**
 * @file include/netinet/tcp.h
 */
#ifndef __NETINET_TCP_H
#define __NETINET_TCP_H

#define TCP_NODELAY      <%=d TCP_NODELAY      %>
#define TCP_MAXSEG       <%=d TCP_MAXSEG       %>
#define TCP_CORK         <%=d TCP_CORK         %>
#define TCP_KEEPIDLE     <%=d TCP_KEEPIDLE     %>
#define TCP_KEEPINTVL    <%=d TCP_KEEPINTVL    %>
#define TCP_KEEPCNT      <%=d TCP_KEEPCNT      %>
#define TCP_SYNCNT       <%=d TCP_SYNCNT       %>
#define TCP_LINGER2      <%=d TCP_LINGER2      %>
#define TCP_DEFER_ACCEPT <%=d TCP_DEFER_ACCEPT %>
#define TCP_WINDOW_CLAMP <%=d TCP_WINDOW_CLAMP %>
#define TCP_INFO         <%=d TCP_INFO         %>
#define TCP_QUICKACK     <%=d TCP_QUICKACK     %>

#endif
