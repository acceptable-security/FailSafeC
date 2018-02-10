/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2008 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/priority.test.c
 */
#include "common.h"
#include <unistd.h>
#include <sys/types.h>
#include <sys/time.h>
#include <sys/resource.h>
#include <errno.h>

/**
 * @testname priority_1
 * @testfor getpriority
 * @testfor setpriority
 */
TEST_CASE(priority_1)
{
  int n, p;

  errno = 0;
  p = getpriority(PRIO_PROCESS, 0);
  TEST_FAIL_IF(errno != 0);

  n = setpriority(PRIO_PROCESS, 0, p + 1);
  TEST_FAIL_IF(n != 0);

  n = setpriority(PRIO_PROCESS, 0, -1);
  TEST_FAIL_IF(geteuid() != 0 && n == 0);

  errno = 0;
  n = getpriority(12345, 0);
  TEST_FAIL_IF(errno == 0);

  n = setpriority(12345, 0, p);
  TEST_FAIL_IF(n == 0);

  TEST(1);
}
