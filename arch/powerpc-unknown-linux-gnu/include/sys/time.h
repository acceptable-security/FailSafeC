/* Generated file -- do not edit. */

/**
 * @file include/sys/time.h
 */
#ifndef __SYS_TIME_H
#define __SYS_TIME_H


#include <sys/__types.h>

#ifndef __TIME_T
#define __TIME_T
typedef __time_t time_t;
#endif

#ifndef __SUSECONDS_T
#define __SUSECONDS_T
typedef __suseconds_t suseconds_t;
#endif

#include <sys/select.h>

struct __fsc_attribute__((named "stdlib_itimerval")) itimerval {
  struct timeval it_interval;
  struct timeval it_value;
};

extern int gettimeofday(struct timeval *time, void *tz);
extern int utimes(const char *, const struct timeval[2]);

#define ITIMER_REAL    0
#define ITIMER_VIRTUAL 1
#define ITIMER_PROF    2

extern int getitimer(int, struct itimerval *);
extern int setitimer(int, const struct itimerval *, struct itimerval *);

#define timerisset(x) ((x)->tv_sec == 0 && (x)->tv_usec == 0)
#define timerclear(x) ((x)->tv_sec = (x)->tv_usec = 0)
#define timercmp(x, y, cp) \
  ((x)->tv_sec == (y)->tv_sec ? (x)->tv_usec cp (y)->tv_usec : (x)->tv_sec cp (y)->tv_sec)

#define timeradd(x, y, r) \
do { \
  (r)->tv_sec = (x)->tv_sec + (y)->tv_sec; \
  (r)->tv_usec = (x)->tv_usec + (y)->tv_usec; \
  if ((r)->tv_usec >= 1000000) { \
    (r)->tv_sec++; \
    (r)->tv_usec -= 1000000; \
  } \
} while (0)
#define timersub(x, y, r) \
do { \
  (r)->tv_sec = (x)->tv_sec - (y)->tv_sec - 1; \
  (r)->tv_usec = (x)->tv_usec - (y)->tv_usec + 1000000; \
  if ((r)->tv_usec >= 1000000) { \
    (r)->tv_sec++; \
    (r)->tv_usec -= 1000000; \
  } \
} while (0)

#endif
