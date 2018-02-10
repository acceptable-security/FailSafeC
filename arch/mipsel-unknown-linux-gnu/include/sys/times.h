/* Generated file -- do not edit. */
/**
 * @file include/sys/times.h
 */
#ifndef __SYS_TIMES_H
#define __SYS_TIMES_H

#include <sys/__types.h>

#ifndef __CLOCK_T
#define __CLOCK_T
typedef __clock_t clock_t;
#endif

struct __fsc_attribute__((named "stdlib_tms")) tms {
  clock_t tms_utime;
  clock_t tms_stime;
  clock_t tms_cutime;
  clock_t tms_cstime;
};

clock_t times(struct tms *);

#endif
