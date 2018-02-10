/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file unix/inttypes.test.c
 */
#include <inttypes.h>
#include <limits.h>

#include "common.h"


/**
 * @testname imaxabs_1
 * @testfor imaxabs
 */
TEST_CASE(imaxabs_1)
{
  TEST_FAIL_IF(imaxabs(0) != 0);
  TEST_FAIL_IF(imaxabs(1) != 1);
  TEST_FAIL_IF(imaxabs(-1) != 1);
  TEST_FAIL_IF(imaxabs(INT_MAX) != INT_MAX);
  TEST_FAIL_IF(imaxabs(-INT_MAX) != INT_MAX);
  TEST(1);
}

static int div_test(intmax_t num, intmax_t denom, intmax_t quot, intmax_t rem)
{
  imaxdiv_t d;
  d = imaxdiv(num, denom);
  return d.quot == quot && d.rem == rem;
}


/**
 * @testname imaxdiv_1
 * @testfor imaxdiv
 */
TEST_CASE(imaxdiv_1)
{
  TEST_FAIL_IF(!div_test(0, 1, 0, 0));
  TEST_FAIL_IF(!div_test(1, 1, 1, 0));
  TEST_FAIL_IF(!div_test(10, 10, 1, 0));
  TEST_FAIL_IF(!div_test(19, 10, 1, 9));
  TEST_FAIL_IF(!div_test(997 * 1007 + 512, 997, 1007, 512));
  TEST(1);
}


/**
 * @testname strtoimax_1
 * @testfor strtoimax
 */
TEST_CASE(strtoimax_1)
{
  char *p = "123abc", *e;
  uintmax_t r;
  r = strtoimax(p, &e, 10);
  TEST_FAIL_IF(r != 123);
  TEST_FAIL_IF(e - p != 3);
  r = strtoimax(p, 0, 10);
  TEST_FAIL_IF(r != 123);
  TEST(1);
}



/**
 * @testname strtoumax_1
 * @testfor strtoumax
 */
TEST_CASE(strtoumax_1)
{
  char *p = "123abc", *e;
  uintmax_t r;
  r = strtoumax(p, &e, 10);
  TEST_FAIL_IF(r != 123);
  TEST_FAIL_IF(e - p != 3);
  r = strtoumax(p, 0, 10);
  TEST_FAIL_IF(r != 123);
  TEST(1);
}

