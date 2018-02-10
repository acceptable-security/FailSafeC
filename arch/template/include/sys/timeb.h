/**
 * @file include/sys/timeb.h
 */
#ifndef __SYS_TIMEB_H
#define __SYS_TIMEB_H

#include <sys/__types.h>

#ifndef __TIME_T
#define __TIME_T
typedef __time_t time_t;
#endif

struct __fsc_attribute__((named "stdlib_timeb")) timeb {
  time_t time;
  unsigned short millitm;
  short timezone;
  short dstflag;
};

extern int ftime(struct timeb *);

#endif
