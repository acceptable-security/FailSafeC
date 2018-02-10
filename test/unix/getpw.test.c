/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/getpw.test.c
 */
#include "common.h"
#include <sys/types.h>
#include <pwd.h>
#include <string.h>

/**
 * @testname getpwnam_1
 * @testfor getpwnam
 */
TEST_CASE(getpwnam_1)
{
  struct passwd *p;
  p = getpwnam("root");
  TEST_FAIL_IF(p == NULL);
  TEST_FAIL_IF(strcmp(p->pw_name, "root") != 0);
  /* TEST_FAIL_IF(strcmp(p->pw_passwd, "x") != 0);  pw_passwd is not in standard. */
  TEST_FAIL_IF(p->pw_uid != 0);
  TEST_FAIL_IF(p->pw_gid != 0);
  TEST_FAIL_IF(strcmp(p->pw_dir, "/root") != 0);
  TEST(1);
}

/**
 * @testname getpwnam_2
 * @testfor getpwnam
 */
TEST_CASE(getpwnam_2)
{
  TEST(getpwnam("nosuchusername") == NULL);
}

/**
 * @testname getpwuid_1
 * @testfor getpwuid
 */
TEST_CASE(getpwuid_1)
{
  struct passwd *p;
  p = getpwuid(0);
  TEST_FAIL_IF(p == NULL);
  TEST_FAIL_IF(strcmp(p->pw_name, "root") != 0);
  /* TEST_FAIL_IF(strcmp(p->pw_passwd, "x") != 0);  pw_passwd is not in standard. */
  TEST_FAIL_IF(p->pw_uid != 0);
  TEST_FAIL_IF(p->pw_gid != 0);
  TEST_FAIL_IF(strcmp(p->pw_dir, "/root") != 0);
  TEST(1);
}

/**
 * @testname getpwuid_2
 * @testfor getpwuid
 */
TEST_CASE(getpwuid_2)
{
  TEST(getpwuid(34567) == NULL);
}

/**
 * @testname endpwent_1
 * @testfor endpwent
 */
TEST_CASE(endpwent_1)
{
  endpwent();
  TEST(1);
}

/**
 * @testname getpwent_1
 * @testfor getpwent
 */
TEST_CASE(getpwent_1)
{
  struct passwd *p;
  int ok = 0;

  while ((p = getpwent()) != NULL) {
    if (p->pw_uid == 0) {
      TEST_FAIL_IF(strcmp(p->pw_name, "root") != 0);
      /* TEST_FAIL_IF(strcmp(p->pw_passwd, "x") != 0);  pw_passwd is not in standard. */
      TEST_FAIL_IF(p->pw_uid != 0);
      TEST_FAIL_IF(p->pw_gid != 0);
      TEST_FAIL_IF(strcmp(p->pw_dir, "/root") != 0);
      ok = 1;
      break; /* Some systems may have 2 or more accounts for UID=0. We hope root is the first one. */
    }
  }
  TEST(ok);
}

/**
 * @testname setpwent_1
 * @testfor setpwent
 */
TEST_CASE(setpwent_1)
{
  struct passwd *p;
  while ((p = getpwent()) != NULL)
    ;
  setpwent();
  TEST_FAIL_IF(getpwent() == NULL);
  TEST(1);
}

