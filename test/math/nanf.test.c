/*
   This file is written by Lepidum Co., Ltd.
   Copyrignt (c) 2005-2008 by Lepidum Co., Ltd.
 */

/**
 * @file math/nanf.test.c
 */
#define _ISOC99_SOURCE
#include <math.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <float.h>
#include "common.h"

static float nanf_strtof(const char *tagp)
{
  char buf[1024];
  sprintf(buf, "NAN(%s)", tagp);
  return strtof(buf, NULL);
}

/**
  * @testname nanf_1
  * @testfor nanf
  */
TEST_CASE(nanf_1)
{
  union {
    float v;
	unsigned char b[sizeof(float)];
  } v1, v2;

  v1.v = nanf("");
  v2.v = nanf_strtof("");
  TEST_FAIL_IF(memcmp(v1.b, v2.b, sizeof(float) != 0));

  v1.v = nanf("0");
  v2.v = nanf_strtof("0");
  TEST_FAIL_IF(memcmp(v1.b, v2.b, sizeof(float) != 0));

  v1.v = nanf("1");
  v2.v = nanf_strtof("1");
  TEST_FAIL_IF(memcmp(v1.b, v2.b, sizeof(float) != 0));

  v1.v = nanf("a");
  v2.v = nanf_strtof("a");
  TEST_FAIL_IF(memcmp(v1.b, v2.b, sizeof(float) != 0));

  TEST(1);
}
