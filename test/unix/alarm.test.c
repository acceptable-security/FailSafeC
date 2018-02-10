/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file unix/alarm.test.c
 */
#include <unistd.h>
#include <signal.h>
#include <time.h>
#include <sys/time.h>

#include "common.h"

static time_t t_start;
static time_t t_end;
static volatile int flag;

static void handler_1(int sig)
{
  TEST_FAIL_IF(sig != SIGALRM);
  TEST_FAIL_IF(flag != 1);
  flag = 0;
}


/**
 * @testname alarm_1
 * @testfor alarm
 */
TEST_CASE(alarm_1)
{
  t_start = time(0);
  flag = 1;
  signal(SIGALRM, handler_1);
  TEST_FAIL_IF(alarm(1) != 0);
  while (flag)
    ;
  t_end = time(0);
  TEST(t_start + 1 == t_end || t_start + 2 == t_end);
}

static void handler_2(int sig)
{
  TEST_FAILED;
}

/**
 * @testname alarm_2
 * @testfor alarm
 */
TEST_CASE(alarm_2)
{
  unsigned int ret;
  t_start = time(0);
  signal(SIGALRM, handler_2);
  TEST_FAIL_IF(alarm(3) != 0);
  ret = alarm(12);
  TEST_FAIL_IF(ret != 3 && ret != 2);
  ret = alarm(0);
  TEST_FAIL_IF(ret != 12 && ret != 11);
  t_start = time(0);
  do {
    t_end = time(0);
  } while (t_end < t_start + 1);
  TEST(1);
}

/**
 * @testname alarm_3
 * @testfor alarm
 */
TEST_CASE_S(alarm_3, SIGALRM)
{
  alarm(1);
  t_start = time(0);
  do {
    t_end = time(0);
  } while (t_end < t_start + 2);
  TEST_FAILED;
}

/**
 * @testname sleep_1
 * @testfor sleep
 */
TEST_CASE(sleep_1)
{
  t_start = time(0);
  TEST_FAIL_IF(sleep(1) != 0);
  t_end = time(0);
  TEST(t_end > t_start);
}

/**
 * @testname usleep_1
 * @testfor usleep
 */
TEST_CASE(usleep_1)
{
  t_start = time(0);
  TEST_FAIL_IF(usleep(1000 * 1000) != 0);
  t_end = time(0);
  TEST(t_end > t_start);
}

static void handler_3(int sig)
{
}

/**
 * @testname pause_1
 * @testfor pause
 */
TEST_CASE(pause_1)
{
  signal(SIGALRM, handler_3);
  alarm(1);
  TEST_FAIL_IF(pause() != -1);
  TEST(1);
}

static void handler_4(int sig)
{
  TEST_FAIL_IF(sig != SIGALRM);
  TEST_FAIL_IF(flag != 1);
  flag = 0;
}

/**
 * @testname ualarm_1
 * @testfor ualarm
 */
TEST_CASE(ualarm_1)
{
  int i;
  t_start = time(0);
  for(i = 0; i < 2; i++) {
    signal(SIGALRM, handler_4);
    flag = 1;
    TEST_FAIL_IF(ualarm(500 * 1000, 0) != 0);
    while (flag);
  }
  t_end = time(0);

  TEST(t_start + 1 == t_end || t_start + 2 == t_end);
}

static void handler_5(int sig)
{
  signal(SIGALRM, handler_5);
  TEST_FAIL_IF(sig != SIGALRM);
  TEST_FAIL_IF(flag == 0);
  --flag;
}

/**
 * @testname ualarm_2
 * @testfor ualarm
 */
TEST_CASE(ualarm_2)
{
  t_start = time(0);
  signal(SIGALRM, handler_5);
  flag = 10;
  TEST_FAIL_IF(ualarm(500 * 1000, 1000 * 100) != 0);
  while (flag);
  TEST_FAIL_IF(ualarm(500 * 1000, 0) == 0);
  t_end = time(0);
  TEST(t_start + 1 == t_end || t_start + 2 == t_end);
}

/**
 * @testname ualarm_3
 * @testfor ualarm
 */
TEST_CASE_S(ualarm_3, SIGALRM)
{
  t_start = time(0);
  ualarm(999 * 1000, 0);
  do {
    t_end = time(0);
  } while (t_end < t_start + 2);
  TEST_FAILED;
}


/**
 * @testname setitimer_getitimer_1
 * @testfor setitimer
 * @testfor getitimer
 */
TEST_CASE(setitimer_getitimer_1)
{
  struct itimerval v1, v2;

  t_start = time(0);
  flag = 1;
  signal(SIGALRM, handler_1);

  v1.it_interval.tv_sec = 0;
  v1.it_interval.tv_usec = 0;
  v1.it_value.tv_sec = 1;
  v1.it_value.tv_usec = 0;
  TEST_FAIL_IF(setitimer(ITIMER_REAL, &v1, &v2) != 0);
  TEST_FAIL_IF(v2.it_interval.tv_sec != 0);
  TEST_FAIL_IF(v2.it_interval.tv_usec != 0);
  TEST_FAIL_IF(v2.it_value.tv_sec != 0);
  TEST_FAIL_IF(v2.it_value.tv_usec != 0);
  while (flag)
    ;
  t_end = time(0);
  TEST_FAIL_IF(getitimer(ITIMER_REAL, &v2) != 0);
  TEST_FAIL_IF(v2.it_interval.tv_sec != 0);
  TEST_FAIL_IF(v2.it_interval.tv_usec != 0);
  TEST_FAIL_IF(v2.it_value.tv_sec != 0);
  TEST_FAIL_IF(v2.it_value.tv_usec != 0);
  TEST_FAIL_IF(t_start + 1 != t_end && t_start + 2 != t_end);
  TEST(1);
}
