/* Generated file -- do not edit. */
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

#define POLLIN     1
#define POLLRDNORM 64
#define POLLRDBAND 128
#define POLLPRI    2
#define POLLOUT    4
#define POLLWRNORM 4
#define POLLWRBAND 256
#define POLLERR    8
#define POLLHUP    16
#define POLLNVAL   32

#endif
