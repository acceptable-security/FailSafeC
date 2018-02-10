/**
 * @file include/sys/select.h
 */
#ifndef __SYS_SELECT_H
#define __SYS_SELECT_H

#include <sys/__types.h>
#include <memory.h>

#ifndef __TIME_T
#define __TIME_T
typedef __time_t time_t;
#endif

#ifndef __SUSECONDS_T
#define __SUSECONDS_T
typedef __suseconds_t suseconds_t;
#endif

typedef char fd_mask;

#define _FDSET_BYTES (1024 / 8)
#define FD_SETSIZE 1024
#define NFDBITS 8

typedef struct __fsc_attribute__((named "stdlib_select_fds")) __select_fds {
    char __fd_set[_FDSET_BYTES];
} fd_set;

struct __fsc_attribute__((named "stdlib_select_timeval")) timeval {
  time_t tv_sec;
  suseconds_t tv_usec;
};

#include <signal.h>
#include <sys/time.h>
#include <time.h>

#define FD_ZERO(ps) memset((ps)->__fd_set, 0, _FDSET_BYTES)
#define FD_ISSET(fd,ps) (((ps)->__fd_set[(fd)/8] >> ((fd) % 8)) & 1)
#define FD_CLR(fd,ps) ((ps)->__fd_set[(fd)/8] &= ~(1 << ((fd) % 8)))
#define FD_SET(fd,ps) ((ps)->__fd_set[(fd)/8] |= (1 << ((fd) % 8)))

int select(int n, fd_set *, fd_set *, fd_set *, struct timeval *);
int pselect(int n, fd_set *, fd_set *, fd_set *, const struct timespec *, const sigset_t *);

#endif
