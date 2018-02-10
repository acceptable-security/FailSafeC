<%#
#include <netinet/in.h>
#%>
/**
 * @file include/netinet/in_internal.h
 */
#ifndef __NETINET_IN_INTERNAL_H
#define __NETINET_IN_INTERNAL_H

#define IPPROTO_IP   <%=d IPPROTO_IP   %>
#define IPPROTO_IPV6 <%=d IPPROTO_IPV6 %>
#define IPPROTO_ICMP <%=d IPPROTO_ICMP %>
#define IPPROTO_RAW  <%=d IPPROTO_RAW  %>
#define IPPROTO_TCP  <%=d IPPROTO_TCP  %>
#define IPPROTO_UDP  <%=d IPPROTO_UDP  %>

#define INADDR_ANY       <%=d INADDR_ANY       %>
#define INADDR_BROADCAST <%=d INADDR_BROADCAST %>

#define IN_LOOPBACKNET <%=d IN_LOOPBACKNET %>

#define IN_CLASSA_NET <%=d IN_CLASSA_NET %>U
#define IN_CLASSB_NET <%=d IN_CLASSB_NET %>U
#define IN_CLASSC_NET <%=d IN_CLASSC_NET %>U

#define IN_CLASSA_HOST <%=d IN_CLASSA_HOST %>U
#define IN_CLASSB_HOST <%=d IN_CLASSB_HOST %>U
#define IN_CLASSC_HOST <%=d IN_CLASSC_HOST %>U

#define IN_CLASSA_NSHIFT <%=d IN_CLASSA_NSHIFT %>
#define IN_CLASSB_NSHIFT <%=d IN_CLASSB_NSHIFT %>
#define IN_CLASSC_NSHIFT <%=d IN_CLASSC_NSHIFT %>

#define IN_CLASSA_MAX <%=d IN_CLASSA_MAX %>
#define IN_CLASSB_MAX <%=d IN_CLASSB_MAX %>

#define INET_ADDRSTRLEN  <%=d INET_ADDRSTRLEN  %>
#define INET6_ADDRSTRLEN <%=d INET6_ADDRSTRLEN %>

#endif
