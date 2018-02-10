/* Generated file -- do not edit. */
/**
 * @file include/sys/socket.h
 */
#ifndef __SOCKET_H
#define __SOCKET_H

/* TODO: CMSG_DATA, CMSG_NXTHDR, CMSG_FIRSTHDR */

#include <sys/__types.h>
#include <socket_internal.h>

#ifndef __SOCKLEN_T
#define __SOCKLEN_T
typedef __socklen_t socklen_t;
#endif

#ifndef __SA_FAMILY_T
#define __SA_FAMILY_T
typedef __sa_family_t sa_family_t;
#endif

#ifndef __SIZE_T
#define __SIZE_T
typedef __size_t size_t;
#endif

#ifndef __SSIZE_T
#define __SSIZE_T
typedef __ssize_t ssize_t;
#endif

#include <sys/uio.h>

struct __fsc_attribute__((named "stdlib_sockaddr")) sockaddr {
  sa_family_t sa_family;
  char sa_data[1];
};

struct __fsc_attribute__((named "stdlib_sockaddr_storage")) sockaddr_storage {
  sa_family_t ss_family;
  char        ss_data[132];
};

struct __fsc_attribute__((named "stdlib_msghdr")) msghdr {
  void         *msg_name;
  socklen_t     msg_namelen;
  struct iovec *msg_iov;
  int           msg_iovlen;
  void         *msg_control;
  socklen_t     msg_controllen;
  int           msg_flags;
};

struct __fsc_attribute__((named "stdlib_cmsghdr")) cmsghdr {
  socklen_t cmsg_len;
  int       cmsg_level;
  int       cmsg_type;
};

struct __fsc_attribute__((named "stdlib_linger")) linger {
  int l_onoff;
  int l_linger;
};

extern int accept(int, struct sockaddr *, socklen_t *);
extern int bind(int, const struct sockaddr *, socklen_t);
extern int connect(int, const struct sockaddr *, socklen_t);
extern int getpeername(int, struct sockaddr *, socklen_t *);
extern int getsockname(int, struct sockaddr *, socklen_t *);
extern int getsockopt(int, int, int, void *, socklen_t *);
extern int listen(int, int);
extern ssize_t recv(int, void *, size_t, int);
extern ssize_t recvfrom(int, void *, size_t, int, struct sockaddr *, socklen_t *);
extern ssize_t recvmsg(int, struct msghdr *, int);
extern ssize_t send(int, const void *, size_t, int);
extern ssize_t sendmsg(int, const struct msghdr *, int);
extern ssize_t sendto(int, const void *, size_t, int, const struct sockaddr *, socklen_t);
extern int setsockopt(int, int, int, const void *, socklen_t);
extern int shutdown(int, int);
extern int socket(int, int, int);
extern int sockatmark(int);
extern int socketpair(int, int, int, int[2]);

#endif
