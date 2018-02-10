/*
   This file is written by Lepidum Co., Ltd.
   Copyrignt (c) 2005-2008 by Lepidum Co., Ltd.
 */

/**
 * @file math/nan.test.c
 */
#define _ISOC99_SOURCE
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include "common.h"

static double nan_strtod(const char *tagp)
{
  char buf[1024];
  sprintf(buf, "NAN(%s)", tagp);
  return strtod(buf, NULL);
}

/**
  * @testname nan_1
  * @testfor nan
  */
TEST_CASE(nan_1)
{
  union {
    double v;
	unsigned char b[sizeof(double)];
  } v1, v2;

  v1.v = nan("");
  v2.v = nan_strtod("");
  TEST_FAIL_IF(memcmp(v1.b, v2.b, sizeof(double) != 0));

  v1.v = nan("0");
  v2.v = nan_strtod("0");
  TEST_FAIL_IF(memcmp(v1.b, v2.b, sizeof(double) != 0));

  v1.v = nan("1");
  v2.v = nan_strtod("1");
  TEST_FAIL_IF(memcmp(v1.b, v2.b, sizeof(double) != 0));

  v1.v = nan("a");
  v2.v = nan_strtod("a");
  TEST_FAIL_IF(memcmp(v1.b, v2.b, sizeof(double) != 0));

  TEST(1);
}
