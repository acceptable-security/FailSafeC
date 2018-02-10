/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/set_id.test.c
 */
#include "common.h"
#include <unistd.h>

/**
 * @testname setuid_1
 * @testfor setuid
 */
TEST_CASE(setuid_1)
{
  TEST_FAIL_IF(setuid(getuid()) != 0);

  TEST(setuid(0) == -1);
}

/**
 * @testname setgid_1
 * @testfor setgid
 */
TEST_CASE(setgid_1)
{
  TEST_FAIL_IF(setgid(getgid()) != 0);

  TEST(setgid(0) == -1);
}

/**
 * @testname setsid_1
 * @testfor setsid
 */
TEST_CASE(setsid_1)
{
  TEST(setsid() != 0);
}
