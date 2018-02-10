<%#
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/param.h>
#include <netdb.h>
#%>
/**
 * @file include/socket_internal.h
 */
#define SCM_RIGHTS  <%=d SCM_RIGHTS %>

#define SOCK_DGRAM      <%=d SOCK_DGRAM     %>
#define SOCK_RAW        <%=d SOCK_RAW       %>
#define SOCK_SEQPACKET  <%=d SOCK_SEQPACKET %>
#define SOCK_STREAM     <%=d SOCK_STREAM    %>

#define SOL_SOCKET  <%=d SOL_SOCKET %>

#define SO_ACCEPTCONN  <%=d SO_ACCEPTCONN %>
#define SO_BROADCAST   <%=d SO_BROADCAST  %>
#define SO_DEBUG       <%=d SO_DEBUG      %>
#define SO_DONTROUTE   <%=d SO_DONTROUTE  %>
#define SO_ERROR       <%=d SO_ERROR      %>
#define SO_KEEPALIVE   <%=d SO_KEEPALIVE  %>
#define SO_LINGER      <%=d SO_LINGER     %>
#define SO_OOBINLINE   <%=d SO_OOBINLINE  %>
#define SO_RCVBUF      <%=d SO_RCVBUF     %>
#define SO_RCVLOWAT    <%=d SO_RCVLOWAT   %>
#define SO_RCVTIMEO    <%=d SO_RCVTIMEO   %>
#define SO_REUSEADDR   <%=d SO_REUSEADDR  %>
#define SO_SNDBUF      <%=d SO_SNDBUF     %>
#define SO_SNDLOWAT    <%=d SO_SNDLOWAT   %>
#define SO_SNDTIMEO    <%=d SO_SNDTIMEO   %>
#define SO_TYPE        <%=d SO_TYPE       %>

#define SOMAXCONN  <%=d SOMAXCONN %>

#define MSG_CTRUNC     <%=d MSG_CTRUNC    %>
#define MSG_DONTROUTE  <%=d MSG_DONTROUTE %>
#define MSG_EOR        <%=d MSG_EOR       %>
#define MSG_OOB        <%=d MSG_OOB       %>
#define MSG_PEEK       <%=d MSG_PEEK      %>
#define MSG_TRUNC      <%=d MSG_TRUNC     %>
#define MSG_WAITALL    <%=d MSG_WAITALL   %>

#define AF_INET    <%=d AF_INET   %>
#define AF_INET6   <%=d AF_INET6  %>
#define AF_UNIX    <%=d AF_UNIX   %>
#define AF_UNSPEC  <%=d AF_UNSPEC %>

/* non-standard */
#define PF_INET    <%=d PF_INET   %>
#define PF_INET6   <%=d PF_INET6  %>
#define PF_UNIX    <%=d PF_UNIX   %>
#define PF_UNSPEC  <%=d PF_UNSPEC %>

#define SHUT_RD    <%=d SHUT_RD   %>
#define SHUT_RDWR  <%=d SHUT_RDWR %>
#define SHUT_WR    <%=d SHUT_WR   %>

/* TODO this should not be here */
#define MAXHOSTNAMELEN  <%=d MAXHOSTNAMELEN %>
