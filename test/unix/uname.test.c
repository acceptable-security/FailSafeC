/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/uname.test.c
 */
#include "common.h"
#include <string.h>
#include <sys/utsname.h>

/**
 * @testname uname_1
 * @testfor uname
 */
TEST_CASE(uname_1)
{
  struct utsname buf;
  uname(&buf);
  TEST_FAIL_IF(strcmp(buf.sysname, getenv("TEST_UNAME_SYSNAME")));
  TEST_FAIL_IF(strcmp(buf.nodename, getenv("TEST_UNAME_NODENAME")));
  TEST_FAIL_IF(strcmp(buf.release, getenv("TEST_UNAME_RELEASE")));
  TEST_FAIL_IF(strcmp(buf.version, getenv("TEST_UNAME_VERSION")));
  TEST_FAIL_IF(strcmp(buf.machine, getenv("TEST_UNAME_MACHINE")));
  TEST(1);
}

/**
 * @testname uname_2
 * @testfor uname
 */
TEST_CASE(uname_2)
{
  TEST_FAIL_IF(uname(NULL) == 0);
  TEST(1);
}

/**
 * @testname uname_1s
 * @testfor uname
 */
TEST_CASE_S(uname_1s, FSC_ABRT)
{
  char buf[8];
  uname((struct utsname*)buf);
  TEST_FAILED;
}
