/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file unix/sysconf.test.c
 */
#include <unistd.h>
#include <stdlib.h>
#include "common.h"

/**
 * @testname sysconf_1
 * @testfor sysconf
 */
TEST_CASE(sysconf_1)
{
  TEST_FAIL_IF(sysconf(-1) != -1);
  TEST(errno == EINVAL);
}
