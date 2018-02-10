/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/gethostname.test.c
 */
#include "common.h"
#include <unistd.h>
#include <string.h>

/**
 * @testname gethostname_1
 * @testfor gethostname
 */
TEST_CASE(gethostname_1)
{
  char buf[1024];

  TEST(gethostname(buf, 1024) == 0);
}

/**
 * @testname gethostname_2_nonlinux
 * @testfor gethostname
 */
TEST_CASE(gethostname_2_nonlinux)
{
  char buf[1024];
  char s;

  TEST_FAIL_IF(gethostname(buf, 1024) != 0);
  s = buf[0];

  memset(buf, 0xff, 1024);
  TEST_FAIL_IF(gethostname(buf, 1) != 0);
  TEST(buf[0] == s || buf[0] == '\0');
}

/**
 * @testname gethostname_2_linux
 * @testfor gethostname
 */
TEST_CASE(gethostname_2_linux)
{
  char buf[1024];
  char s;

  TEST_FAIL_IF(gethostname(buf, 1024) != 0);
  s = buf[0];

  memset(buf, 0xff, 1024);
  /* TEST_FAIL_IF(gethostname(buf, 1) != 0); */
  /* On linux/i386, this operation will return -1 */
  gethostname(buf, 1);
  TEST(buf[0] == s || buf[0] == '\0');
}
