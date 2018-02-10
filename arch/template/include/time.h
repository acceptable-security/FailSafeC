/**
 * @file include/time.h
 */
#ifndef _TIME_H_
#define _TIME_H_

#include <time_internal.h>
#include <sys/__types.h>

#ifndef __SIZE_T
#define __SIZE_T
typedef __size_t size_t;
#endif

#ifndef __TIME_T
#define __TIME_T
typedef __time_t time_t;
#endif

#ifndef __CLOCK_T
#define __CLOCK_T
typedef __clock_t clock_t;
#endif

#include <signal.h>

struct __fsc_attribute__((named "stdlib_tm")) tm {
    int tm_sec, tm_min, tm_hour, tm_mday, tm_mon, tm_year, tm_wday, tm_yday, tm_isdst;
};

struct __fsc_attribute__((named "stdlib_timespec")) timespec {
  time_t tv_sec;
  long tv_nsec;
};

#include <signal.h>

extern time_t time(time_t *);
extern struct tm *localtime(const time_t *);
extern char *asctime(const struct tm *);
extern char *ctime(const time_t *);
extern clock_t clock(void);

extern struct tm *gmtime(const time_t *);
extern struct tm *gmtime_r(const time_t *, struct tm *);

extern long int timezone;
extern int daylight;
extern char *tzname[2];
extern void tzset(void);

extern size_t strftime(char *, size_t, const char *, const struct tm *);

extern double difftime(time_t, time_t);
extern struct tm *getdate(const char *);
extern time_t mktime(struct tm *);
extern char *strptime(const char *, const char *, struct tm *);
#endif
