/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"

#include <sys/time.h>
#include <sys/stat.h>
#include <utime.h>
#include <time.h>

/**
 * @testname utimes_1
 * @testfor utimes
 */
TEST_CASE(utimes_1)
{
  struct timeval t[2];
  struct stat s;

  t[0].tv_sec = 10000000;
  t[0].tv_usec = 0;

  t[1].tv_sec = 20000000;
  t[1].tv_usec = 0;

  TEST_FAIL_IF(utimes(TEST_STDOUT, t) != 0);
  TEST_FAIL_IF(stat(TEST_STDOUT, &s) != 0);

  TEST(s.st_atime == 10000000 && s.st_mtime == 20000000);
}

#include <stdio.h>

/**
 * @testname utimes_2
 * @testfor utimes
 */
TEST_CASE(utimes_2)
{
  struct timeval t[2] = { { 0, 0 }, { 0, 0 } };
  struct stat s;
  time_t now = time(NULL);

  TEST_FAIL_IF(utimes(TEST_STDOUT, t) != 0);
  TEST_FAIL_IF(stat(TEST_STDOUT, &s) != 0);
  TEST_FAIL_IF(s.st_atime != 0 || s.st_mtime != 0);

  TEST_FAIL_IF(utimes(TEST_STDOUT, NULL) != 0);
  TEST_FAIL_IF(stat(TEST_STDOUT, &s) != 0);
  TEST(s.st_atime >= now && s.st_mtime >= now);
}

/**
 * @testname utime_1
 * @testfor utime
 */
TEST_CASE(utime_1)
{
  struct utimbuf t;
  struct stat s;

  t.actime = 10000000;
  t.modtime = 20000000;
  TEST_FAIL_IF(utime(TEST_STDOUT, &t) != 0);
  TEST_FAIL_IF(stat(TEST_STDOUT, &s) != 0);

  TEST(s.st_atime == 10000000 && s.st_mtime == 20000000);
}

/**
 * @testname utime_2
 * @testfor utime
 */
TEST_CASE(utime_2)
{
  struct utimbuf t;
  struct stat s;
  time_t now = time(NULL);

  TEST_FAIL_IF(utime(TEST_STDOUT, NULL) != 0);
  TEST_FAIL_IF(stat(TEST_STDOUT, &s) != 0);
  TEST(s.st_atime >= now && s.st_mtime >= now);
}
