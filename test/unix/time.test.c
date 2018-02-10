/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file unix/time.test.c
 */
#include <time.h>
#include <stdio.h>
#include <sys/timeb.h>
#include <sys/times.h>
#include <sys/time.h>
#include "common.h"

#include <string.h>
#include <limits.h>

static struct timeb tb;
static time_t t;
static struct tms tms;

/**
 * @testname time_1
 * @testfor time
 */
TEST_CASE(time_1)
{
  time_t t2 = time(&t);
  TEST_FAIL_IF(t2 == (time_t)-1);
  TEST(t == t2);
}

/**
 * @testname time_2
 * @testfor time
 */
TEST_CASE(time_2)
{
  t = time(0);
  TEST(t != (time_t)-1);
}

/**
 * @testname ftime_1
 * @testfor ftime
 */
TEST_CASE(ftime_1)
{
  TEST_FAIL_IF(ftime(&tb) != 0);
  TEST_FAIL_IF(time(&t) == (time_t)-1);
  TEST_FAIL_IF(t < tb.time);
  TEST_FAIL_IF(t > tb.time + 1);
  TEST_FAIL_IF(tb.millitm >= 1000);
  TEST_FAIL_IF(tb.timezone > +14 * 60);
  TEST_FAIL_IF(tb.timezone < -11 * 60);
  TEST(1);
}

/**
 * @testname gmtime_r_1
 * @testfor gmtime_r
 */
TEST_CASE(gmtime_r_1)
{
  struct tm r;
  t = 0;
  TEST_FAIL_IF(gmtime_r(&t, &r) == 0);
  TEST_FAIL_IF(r.tm_sec != 0);
  TEST_FAIL_IF(r.tm_min != 0);
  TEST_FAIL_IF(r.tm_hour != 0);
  TEST_FAIL_IF(r.tm_mday != 1);
  TEST_FAIL_IF(r.tm_mon != 0);
  TEST_FAIL_IF(r.tm_year != 70);
  TEST_FAIL_IF(r.tm_wday != 4);
  TEST_FAIL_IF(r.tm_yday != 0);
  TEST_FAIL_IF(r.tm_isdst != 0);
  TEST(1);
}

/**
 * @testname gmtime_r_2
 * @testfor gmtime_r
 */
TEST_CASE(gmtime_r_2)
{
  struct tm r;
  t = 1138177035;
  TEST_FAIL_IF(gmtime_r(&t, &r) == 0);
  TEST_FAIL_IF(r.tm_sec != 15);
  TEST_FAIL_IF(r.tm_min != 17);
  TEST_FAIL_IF(r.tm_hour != 8);
  TEST_FAIL_IF(r.tm_mday != 25);
  TEST_FAIL_IF(r.tm_mon != 0);
  TEST_FAIL_IF(r.tm_year != 106);
  TEST_FAIL_IF(r.tm_wday != 3);
  TEST_FAIL_IF(r.tm_yday != 24);
  TEST_FAIL_IF(r.tm_isdst != 0);
  TEST(1);
}

/**
 * @testname localtime_1
 * @testfor localtime
 */
TEST_CASE(localtime_1)
{
  struct tm *r;
  putenv("TZ=JST-9");
  t = 0;
  r = localtime(&t);
  TEST_FAIL_IF(r == 0);
  TEST_FAIL_IF(r->tm_sec != 0);
  TEST_FAIL_IF(r->tm_min != 0);
  TEST_FAIL_IF(r->tm_hour != 9);
  TEST_FAIL_IF(r->tm_mday != 1);
  TEST_FAIL_IF(r->tm_mon != 0);
  TEST_FAIL_IF(r->tm_year != 70);
  TEST_FAIL_IF(r->tm_wday != 4);
  TEST_FAIL_IF(r->tm_yday != 0);
  TEST_FAIL_IF(r->tm_isdst != 0);
  TEST(1);
}

/**
 * @testname localtime_2
 * @testfor localtime
 */
TEST_CASE(localtime_2)
{
  struct tm *r;
  putenv("TZ=JST-9");
  t = 1138177035 - 3600 * 9;
  r = localtime(&t);
  TEST_FAIL_IF(r == 0);
  TEST_FAIL_IF(r->tm_sec != 15);
  TEST_FAIL_IF(r->tm_min != 17);
  TEST_FAIL_IF(r->tm_hour != 8);
  TEST_FAIL_IF(r->tm_mday != 25);
  TEST_FAIL_IF(r->tm_mon != 0);
  TEST_FAIL_IF(r->tm_year != 106);
  TEST_FAIL_IF(r->tm_wday != 3);
  TEST_FAIL_IF(r->tm_yday != 24);
  TEST_FAIL_IF(r->tm_isdst != 0);
  TEST(1);
}

/**
 * @testname time_1s
 * @testfor time
 */
TEST_CASE_S(time_1s, FSC_ABRT)
{
  char a;
  time((time_t*)&a);
}

/**
 * @testname ftime_1s
 * @testfor ftime
 */
TEST_CASE_S(ftime_1s, FSC_ABRT)
{
  ftime(NULL);
}

/**
 * @testname ftime_2s
 * @testfor ftime
 */
TEST_CASE_S(ftime_2s, FSC_ABRT)
{
  char a;
  ftime((struct timeb *)&a);
}

/**
 * @testname times_1s
 * @testfor times
 */
TEST_CASE_S(times_1s, FSC_ABRT)
{
  times(NULL);
}

/**
 * @testname times_2s
 * @testfor times
 */
TEST_CASE_S(times_2s, FSC_ABRT)
{
  char a;
  times((struct tms *)&a);
}

/**
 * @testname gmtime_1s
 * @testfor gmtime
 */
TEST_CASE_S(gmtime_1s, FSC_ABRT)
{
  gmtime(NULL);
}

/**
 * @testname gmtime_2s
 * @testfor gmtime
 */
TEST_CASE_S(gmtime_2s, FSC_ABRT)
{
  char a;
  gmtime((time_t *)&a);
}

/**
 * @testname gmtime_r_1s
 * @testfor gmtime_r
 */
TEST_CASE_S(gmtime_r_1s, FSC_ABRT)
{
  struct tm r;
  gmtime_r(NULL, &r);
}

/**
 * @testname gmtime_r_2s
 * @testfor gmtime_r
 */
TEST_CASE_S(gmtime_r_2s, FSC_ABRT)
{
  struct tm r;
  char a;
  gmtime_r((time_t *)&a, &r);
}

/**
 * @testname gmtime_r_3s
 * @testfor gmtime_r
 */
TEST_CASE_S(gmtime_r_3s, FSC_ABRT)
{
  t = 1138177035;
  gmtime_r(&t, NULL);
}

/**
 * @testname gmtime_r_4s
 * @testfor gmtime_r
 */
TEST_CASE_S(gmtime_r_4s, FSC_ABRT)
{
  char a;
  t = 1138177035;
  gmtime_r(&t, (struct tm *)&a);
}

/**
 * @testname strftime_1
 * @testfor strftime
 */
TEST_CASE(strftime_1)
{
  char buf[1024];

  struct tm r;
  t = 1138177035;
  gmtime_r(&t, &r);

  TEST_FAIL_IF(strftime(buf, 1024, "%Y-%m-%d", &r) != 10);
  TEST_FAIL_IF(strcmp("2006-01-25", buf) != 0);

  memset(buf, 0xff, 1024);
  TEST_FAIL_IF(strftime(buf, 10, "%Y-%m-%d", &r) != 0);
  TEST(1);
}

/**
 * @testname gettimeofday_1
 * @testfor gettimeofday
 */
TEST_CASE(gettimeofday_1)
{
  struct timeval tv = { 0, 1000000 };

  TEST_FAIL_IF(gettimeofday(&tv, NULL) != 0);

  TEST_FAIL_IF(tv.tv_sec == 0);
  TEST_FAIL_IF(tv.tv_usec == 1000000);

  TEST(1);
}

/**
 * @testname tzset_1
 * @testfor tzset
 */
TEST_CASE(tzset_1)
{
  tzname[0] = tzname[1] = (void*)42;
  timezone  = 172800L;
  daylight =  -1;

  tzset();

  TEST_FAIL_IF(tzname[0] == (void*)42);
  TEST_FAIL_IF(tzname[1] == (void*)42);
  TEST_FAIL_IF(timezone == 172800L);
  TEST_FAIL_IF(daylight == -1);

  TEST(1);
}

/**
 * @testname getdate_1
 * @testfor getdate
 */
TEST_CASE(getdate_1)
{
  char *datemsk = getenv("DATEMSK");
  FILE *f;
  struct tm *t;

  TEST_FAIL_IF(datemsk == NULL);
  f = fopen(datemsk, "w");
  TEST_FAIL_IF(f == NULL);
  fputs("%m\n", f);
  fputs("%A %B %d, %Y, %H:%M:%S\n", f);
  fputs("run job at %I %p, %B %dnd\n", f);
  fclose(f);

  t = getdate("10");
  TEST_FAIL_IF(t->tm_mon != 9);
  t = getdate("Tuesday September 25, 2007, 16:27:50");
  TEST_FAIL_IF(t->tm_wday != 2);
  TEST_FAIL_IF(t->tm_mon != 8);
  TEST_FAIL_IF(t->tm_mday != 25);
  TEST_FAIL_IF(t->tm_year != 107);
  TEST_FAIL_IF(t->tm_hour != 16);
  TEST_FAIL_IF(t->tm_min != 27);
  TEST_FAIL_IF(t->tm_sec != 50);
  t = getdate("run job at 4 PM, September 25nd");
  TEST_FAIL_IF(t->tm_mon != 8);
  TEST_FAIL_IF(t->tm_mday != 25);
  TEST_FAIL_IF(t->tm_hour != 16);
  TEST(1);
}

/**
 * @testname difftime_1
 * @testfor difftime
 */
TEST_CASE(difftime_1)
{
  double r;

  r = difftime(0, 0);
  TEST_FAIL_IF(r != 0.0);
  r = difftime(1, 0);
  TEST_FAIL_IF(r != 1.0);
  r = difftime(0, 1);
  TEST_FAIL_IF(r != -1.0);
  TEST(1);
}

/**
 * @testname strptime_1
 * @testfor strptime
 */
TEST_CASE(strptime_1)
{
  char *s, *r;
  struct tm t;

  s = "10x";
  r = strptime(s, "%m", &t);
  TEST_FAIL_IF(r != s + 2);
  TEST_FAIL_IF(t.tm_mon != 9);
  s = "Tuesday September 25, 2007, 16:27:50x";
  r = strptime(s, "%A %B %d, %Y, %H:%M:%S", &t);
  TEST_FAIL_IF(r != s + 36);
  TEST_FAIL_IF(t.tm_wday != 2);
  TEST_FAIL_IF(t.tm_mon != 8);
  TEST_FAIL_IF(t.tm_mday != 25);
  TEST_FAIL_IF(t.tm_year != 107);
  TEST_FAIL_IF(t.tm_hour != 16);
  TEST_FAIL_IF(t.tm_min != 27);
  TEST_FAIL_IF(t.tm_sec != 50);
  s = "run job at 4 PM, September 25ndx";
  r = strptime(s, "run job at %I %p, %B %dnd", &t);
  TEST_FAIL_IF(r != s + 31);
  TEST_FAIL_IF(t.tm_mon != 8);
  TEST_FAIL_IF(t.tm_mday != 25);
  TEST_FAIL_IF(t.tm_hour != 16);
  TEST(1);
}

/**
 * @testname clock_1
 * @testfor clock
 */
TEST_CASE(clock_1)
{
  clock_t a, b;
  time_t t;
  a = clock();
  t = time(NULL);
  while (time(NULL) - t < 2)
    ;
  b = clock();
  TEST(b - a >= 1 * CLOCKS_PER_SEC);
}

/**
 * @testname mktime_1
 * @testfor mktime
 */
TEST_CASE(mktime_1)
{
  struct tm tm;
  time_t t = 0;
  TEST_FAIL_IF(mktime(localtime(&t)) != 0);
  t = time(NULL);
  TEST_FAIL_IF(time(NULL) - mktime(localtime(&t)) > 1);
  TEST(1);
}

/**
 * @testname mktime_2
 * @testfor mktime
 */
TEST_CASE(mktime_2)
{
  struct tm tm = {0};
  /* 2000-02-30 */
  tm.tm_year = 100;
  tm.tm_mon  = 1;
  tm.tm_mday = 30;
  TEST_FAIL_IF(mktime(&tm) == -1);
  TEST(tm.tm_mon==2 && tm.tm_mday==1 && tm.tm_wday==3 && tm.tm_yday==60);
}

