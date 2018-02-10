/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file unix/get_id.test.c
 */
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>

#include "common.h"

static int cmpenv(char *name, int value)
{
  char buf[256];
  char *p = getenv(name);
  sprintf(buf, "%d", value);
  if (p && strcmp(p, buf) == 0)
    return 1;
  else
    return 0;
}

/**
 * @testname getpid_1
 * @testfor getpid
 */
TEST_CASE(getpid_1)
{
  TEST(cmpenv("TEST_PID", getpid()));
}

/**
 * @testname getppid_1
 * @testfor getppid
 */
TEST_CASE(getppid_1)
{
  TEST(cmpenv("TEST_PPID", getppid()));
}

/**
 * @testname getuid_1
 * @testfor getuid
 */
TEST_CASE(getuid_1)
{
  TEST(cmpenv("TEST_UID", getuid()));
}

/**
 * @testname getgid_1
 * @testfor getgid
 */
TEST_CASE(getgid_1)
{
  TEST(cmpenv("TEST_GID", getgid()));
}

/**
 * @testname geteuid_1
 * @testfor geteuid
 */
TEST_CASE(geteuid_1)
{
  TEST(cmpenv("TEST_EUID", geteuid()));
}

/**
 * @testname getegid_1
 * @testfor getegid
 */
TEST_CASE(getegid_1)
{
  TEST(cmpenv("TEST_EGID", getegid()));
}

/**
 * @testname getgroups_1
 * @testfor getgroups
 */
TEST_CASE(getgroups_1)
{
  getgroups(0, NULL);
  TEST(1);
}

/**
 * @testname getsid_1
 * @testfor getsid
 */
TEST_CASE(getsid_1)
{
  TEST(cmpenv("TEST_SID", getsid(0)));
}

/**
 * @testname getpgrp_1
 * @testfor getpgrp
 */
TEST_CASE(getpgrp_1)
{
  TEST(cmpenv("TEST_PGRP", getpgrp()));
}

/**
 * @testname getlogin_1
 * @testfor getlogin
 */
TEST_CASE(getlogin_1)
{
  char *p = getlogin();
  TEST(strcmp(getenv("TEST_LOGINNAME"), p) == 0);
}

/**
 * @testname getlogin_r_1
 * @testfor getlogin_r
 */
TEST_CASE(getlogin_r_1)
{
  char buf[256];
  TEST_FAIL_IF(getlogin_r(buf, 256) != 0);
  TEST(strcmp(getenv("TEST_LOGINNAME"), buf) == 0);
}

/**
 * @testname getlogin_r_2
 * @testfor getlogin_r
 */
TEST_CASE(getlogin_r_2)
{
  char buf[1];
  TEST(getlogin_r(buf, 1) != 0);
}

/**
 * @testname getpgid_1
 * @testfor getpgid
 */
TEST_CASE(getpgid_1)
{
  TEST(cmpenv("TEST_PGID", getpgid(0)));
}
