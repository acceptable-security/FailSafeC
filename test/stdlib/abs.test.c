/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2008 by Lepidum Co., Ltd.
 */

/**
 * @file stdlib/abs.test.c
 */

#include "common.h"
#include <stdlib.h>
#include <limits.h>

/**
 * @testname abs_1
 * @testfor abs
 */
TEST_CASE(abs_1)
{
  TEST_FAIL_IF(abs(0) != 0);
  TEST_FAIL_IF(abs(1) != 1);
  TEST_FAIL_IF(abs(12345) != 12345);
  TEST_FAIL_IF(abs(INT_MAX) != INT_MAX);
  TEST_FAIL_IF(abs(-1) != 1);
  TEST_FAIL_IF(abs(-12345) != 12345);
  TEST_FAIL_IF(abs(INT_MIN + 1) != -(INT_MIN + 1));
  TEST(1);
}

/**
 * @testname labs_1
 * @testfor labs
 */
TEST_CASE(labs_1)
{
  TEST_FAIL_IF(labs(0) != 0);
  TEST_FAIL_IF(labs(1) != 1);
  TEST_FAIL_IF(labs(12345) != 12345);
  TEST_FAIL_IF(labs(LONG_MAX) != LONG_MAX);
  TEST_FAIL_IF(labs(-1) != 1);
  TEST_FAIL_IF(labs(-12345) != 12345);
  TEST_FAIL_IF(labs(LONG_MIN + 1) != -(LONG_MIN + 1));
  TEST(1);
}

/**
 * @testname llabs_1
 * @testfor llabs
 */
TEST_CASE(llabs_1)
{
  TEST_FAIL_IF(llabs(0) != 0);
  TEST_FAIL_IF(llabs(1) != 1);
  TEST_FAIL_IF(llabs(12345) != 12345);
  TEST_FAIL_IF(llabs(LLONG_MAX) != LLONG_MAX);
  TEST_FAIL_IF(llabs(-1) != 1);
  TEST_FAIL_IF(llabs(-12345) != 12345);
  TEST_FAIL_IF(llabs(LLONG_MIN + 1) != -(LLONG_MIN + 1));
  TEST(1);
}
