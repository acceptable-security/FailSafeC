/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/nice.test.c
 */
#include "common.h"
#include <unistd.h>
#include <errno.h>

/**
 * @testname nice_1
 * @testfor nice
 */
TEST_CASE(nice_1)
{
  int p = nice(0);

  TEST_FAIL_IF(p != nice(0));
  TEST(p + 1 == nice(1));
}

/**
 * @testname nice_2
 * @testfor nice
 */
TEST_CASE(nice_2)
{
  errno = 0;
  TEST_FAIL_IF(nice(-1) != -1);
  TEST(errno != 0);
}
