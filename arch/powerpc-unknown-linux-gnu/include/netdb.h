/* Generated file -- do not edit. */
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

#define IPPORT_RESERVED 1024

#define HOST_NOT_FOUND 1
#define NO_DATA        4
#define NO_RECOVERY    3
#define TRY_AGAIN      2

#define AI_PASSIVE     1
#define AI_CANONNAME   2
#define AI_NUMERICHOST 4

#define AI_NUMERICSERV 1024

#define AI_V4MAPPED    8
#define AI_ALL         16
#define AI_ADDRCONFIG  32

#define NI_NUMERICHOST  1

#define NI_NUMERICSERV  2

#define NI_NOFQDN       4
#define NI_NAMEREQD     8
#define NI_DGRAM        16


#define EAI_AGAIN    -3
#define EAI_BADFLAGS -1
#define EAI_FAIL     -4
#define EAI_FAMILY   -6
#define EAI_MEMORY   -10
#define EAI_NONAME   -2

#define EAI_OVERFLOW -12

#define EAI_SERVICE  -8
#define EAI_SOCKTYPE -7
#define EAI_SYSTEM   -11

#define NETDB_INTERNAL -1
#define NETDB_SUCCESS  0
#define HOST_NOT_FOUND 1
#define TRY_AGAIN      2
#define NO_RECOVERY    3
#define NO_DATA        4
#define NO_ADDRESS     4

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
