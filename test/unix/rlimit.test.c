/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/rlimit.test.c
 */
#include "common.h"
#include <sys/resource.h>

/**
 * @testname rlimit_1
 * @testfor setrlimit
 * @testfor getrlimit
 */
TEST_CASE(rlimit_1)
{
  struct rlimit lim0, lim1, lim2;
  int n;
  n = getrlimit(RLIMIT_NOFILE, &lim0);
  TEST_FAIL_IF(n != 0);
  lim1.rlim_cur = lim0.rlim_cur - 1;
  lim1.rlim_max = lim0.rlim_max - 1;
  n = setrlimit(RLIMIT_NOFILE, &lim1);
  TEST_FAIL_IF(n != 0);
  n = getrlimit(RLIMIT_NOFILE, &lim2);
  TEST_FAIL_IF(n != 0);
  TEST_FAIL_IF(lim2.rlim_cur != lim1.rlim_cur);
  TEST_FAIL_IF(lim2.rlim_max != lim1.rlim_max);
  TEST(1);
}

