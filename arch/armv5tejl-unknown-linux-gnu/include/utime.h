/* Generated file -- do not edit. */
#ifndef __UTIME_H
#define __UTIME_H

#include <sys/__types.h>

#ifndef __TIME_T
#define __TIME_T
typedef __time_t time_t;
#endif

struct __fsc_attribute__((named "stdlib_utimbuf")) utimbuf {
  time_t actime;
  time_t modtime;
};

extern int utime(const char *, const struct utimbuf *);

#endif
