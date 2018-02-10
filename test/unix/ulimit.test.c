/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2008 by Lepidum Co., Ltd.
 */

/**
 * @file unix/ulimit.test.c
 */
#include <limits.h>
#include <ulimit.h>

#include "common.h"

/**
 * @testname ulimit_1
 * @testfor ulimit
 */
TEST_CASE(ulimit_1)
{
  long l;
  l = ulimit(UL_GETFSIZE);
  TEST_FAIL_IF(ulimit(UL_SETFSIZE, l) != 0);
  TEST_FAIL_IF(ulimit(UL_SETFSIZE, 0) != 0);
  TEST_FAIL_IF(ulimit(UL_GETFSIZE) != 0);
  TEST_FAIL_IF(ulimit(UL_SETFSIZE, 0x7fffff) == 0);
  TEST_FAIL_IF(ulimit(UL_GETFSIZE) != 0);
  TEST(1);
}
