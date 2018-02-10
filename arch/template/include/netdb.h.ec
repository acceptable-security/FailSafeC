<%#shared /* -*- c -*- */
#include <netdb.h>
#%>
/**
 * @file include/netdb.h
 */
#ifndef __NETDB_H
#define __NETDB_H

#include <netinet/in.h>
#include <sys/socket.h>
#include <stdint.h>

extern int *__h_errno(void);

#define h_errno (*__h_errno())

#define IPPORT_RESERVED <%=d IPPORT_RESERVED %>

#define HOST_NOT_FOUND <%=d HOST_NOT_FOUND %>
#define NO_DATA        <%=d NO_DATA        %>
#define NO_RECOVERY    <%=d NO_RECOVERY    %>
#define TRY_AGAIN      <%=d TRY_AGAIN      %>

#define AI_PASSIVE     <%=d AI_PASSIVE     %>
#define AI_CANONNAME   <%=d AI_CANONNAME   %>
#define AI_NUMERICHOST <%=d AI_NUMERICHOST %>
<%(void)[[?%>
#define AI_NUMERICSERV <%=d AI_NUMERICSERV %>
<%]];%>
#define AI_V4MAPPED    <%=d AI_V4MAPPED    %>
#define AI_ALL         <%=d AI_ALL         %>
#define AI_ADDRCONFIG  <%=d AI_ADDRCONFIG  %>

#define NI_NUMERICHOST  <%=d NI_NUMERICHOST  %>
<%(void)[[?%>
#define NI_NUMERICSERV  <%=d NI_NUMERICSERV  %>
<%]];%>
#define NI_NOFQDN       <%=d NI_NOFQDN       %>
#define NI_NAMEREQD     <%=d NI_NAMEREQD     %>
#define NI_DGRAM        <%=d NI_DGRAM        %>
<%(void)[[?%>
#define NI_NUMERICSCOPE <%=d NI_NUMERICSCOPE %>
<%]];%>

#define EAI_AGAIN    <%=d EAI_AGAIN    %>
#define EAI_BADFLAGS <%=d EAI_BADFLAGS %>
#define EAI_FAIL     <%=d EAI_FAIL     %>
#define EAI_FAMILY   <%=d EAI_FAMILY   %>
#define EAI_MEMORY   <%=d EAI_MEMORY   %>
#define EAI_NONAME   <%=d EAI_NONAME   %>
<%(void)[[?%>
#define EAI_OVERFLOW <%=d EAI_OVERFLOW %>
<%]];%>
#define EAI_SERVICE  <%=d EAI_SERVICE  %>
#define EAI_SOCKTYPE <%=d EAI_SOCKTYPE %>
#define EAI_SYSTEM   <%=d EAI_SYSTEM   %>

#define NETDB_INTERNAL <%=d NETDB_INTERNAL %>
#define NETDB_SUCCESS  <%=d NETDB_SUCCESS %>
#define HOST_NOT_FOUND <%=d HOST_NOT_FOUND %>
#define TRY_AGAIN      <%=d TRY_AGAIN %>
#define NO_RECOVERY    <%=d NO_RECOVERY %>
#define NO_DATA        <%=d NO_DATA %>
#define NO_ADDRESS     <%=d NO_ADDRESS %>

struct __fsc_attribute__((named "stdlib_hostent")) hostent {
  char *h_name;
  char **h_aliases;
  int h_addrtype;
  int h_length;
  char **h_addr_list;
};

struct __fsc_attribute__((named "stdlib_netent")) netent {
  char *n_name;
  char **n_aliases;
  int n_addrtype;
  uint32_t n_net;
};

struct __fsc_attribute__((named "stdlib_servent")) servent {
  char *s_name;
  char **s_aliases;
  int s_port;
  char *s_proto;
};

struct __fsc_attribute__((named "stdlib_protoent")) protoent {
  char *p_name;
  char **p_aliases;
  int p_proto;
};

struct __fsc_attribute__((named "stdlib_addrinfo")) addrinfo {
  int ai_flags;
  int ai_family;
  int ai_socktype;
  int ai_protocol;
  socklen_t ai_addrlen;
  struct sockaddr *ai_addr;
  char *ai_canonname;
  struct addrinfo *ai_next;
};

#define h_addr h_addr_list[0]

extern const char *gai_strerror(int);

extern void endhostent(void);
extern struct hostent *gethostbyaddr(const void *, socklen_t, int);
extern struct hostent *gethostbyname(const char *);
extern struct hostent *gethostent(void);
extern void sethostent(int);

extern void endnetent(void);
extern struct netent *getnetbyaddr(uint32_t net, int type);
extern struct netent *getnetbyname(const char *);
extern struct netent *getnetent(void);
extern void setnetent(int);

extern void endprotoent(void);
extern struct protoent *getprotobyname(const char *);
extern struct protoent *getprotobynumber(int);
extern struct protoent *getprotoent(void);
extern void setprotoent(int);

extern void endservent(void);
extern struct servent *getservbyname(const char *, const char *);
extern struct servent *getservbyport(int, const char *);
extern struct servent *getservent(void);
extern void setservent(int);

extern void freeaddrinfo(struct addrinfo *);
extern int getaddrinfo(const char *, const char *, const struct addrinfo *, struct addrinfo **);
extern int getnameinfo(const struct sockaddr *, socklen_t, char *, socklen_t, char *, socklen_t, int);

#endif
