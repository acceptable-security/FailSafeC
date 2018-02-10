/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file unix/ioctl.test.c
 */
#include <unistd.h>
#include <sys/ioctl.h>

#include "common.h"

/**
 * @testname ioctl_FIONREAD
 * @testfor ioctl
 */
TEST_CASE(ioctl_FIONREAD)
{
  int p[2];
  int v = 128;
  
  TEST_FAIL_IF(pipe(p) < 0);
  TEST_FAIL_IF(ioctl(p[0], FIONREAD, &v) < 0);
  TEST_FAIL_IF(v != 0);
  TEST_FAIL_IF(write(p[1], "hello", 5) != 5);
  TEST_FAIL_IF(ioctl(p[0], FIONREAD, &v) < 0);
  TEST_FAIL_IF(v != 5);
  TEST(1);
}
