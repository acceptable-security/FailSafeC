/* Generated file -- do not edit. */
/**
 * @file include/sys/resource.h
 */

#ifndef __SYS_RESOURCE_H
#define __SYS_RESOURCE_H

#include <sys/__types.h>

#ifndef __ID_T
#define __ID_T
typedef __id_t id_t;
#endif

#ifndef __RLIM_T
#define __RLIM_T
typedef __rlim_t rlim_t;
#endif

#include <sys/time.h>

#define PRIO_PROCESS 0
#define PRIO_PGRP    1
#define PRIO_USER    2

#define RLIM_INFINITY  -1
#define RLIM_SAVED_MAX -1
#define RLIM_SAVED_CUR -1

#define RUSAGE_SELF     0
#define RUSAGE_CHILDREN -1

#define RLIMIT_CORE   4
#define RLIMIT_CPU    0
#define RLIMIT_DATA   2
#define RLIMIT_FSIZE  1
#define RLIMIT_NOFILE 7
#define RLIMIT_STACK  3
#define RLIMIT_AS     9

struct __fsc_attribute__((named "stdlib_rlimit")) rlimit {
  rlim_t rlim_cur;
  rlim_t rlim_max;
};

struct __fsc_attribute__((named "stdlib_rusage")) rusage {
  struct timeval ru_utime;
  struct timeval ru_stime;
};

extern int getpriority(int, id_t);
extern int getrlimit(int, struct rlimit *);
extern int getrusage(int, struct rusage *);
extern int setpriority(int, id_t, int);
extern int setrlimit(int, const struct rlimit *);

#endif
