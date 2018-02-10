/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/root.test.c
 */
#include "common.h"
#include <unistd.h>
#include <errno.h>

/**
 * @testname chroot_1
 * @testfor chroot
 *
 * @todo require root privilege.
 */
TEST_CASE(chroot_1)
{
  int ret;
  TEST_FAIL_IF(geteuid() == 0);
  ret = chroot("/usr");
  TEST_FAIL_IF(ret != -1);
  TEST(errno == EPERM);
}

/**
 * @testname initgroups_1
 * @testfor initgroups
 *
 * @todo require root privilege.
 */
TEST_CASE(initgroups_1)
{
  int ret;
  TEST_FAIL_IF(geteuid() == 0);
  ret = initgroups("nobody", -1);
  TEST_FAIL_IF(ret != -1);
  TEST(errno == EPERM);
}

/**
 * @testname setgroups_1
 * @testfor setgroups
 *
 * @todo require root privilege.
 */
TEST_CASE(setgroups_1)
{
  int ret;
  gid_t list[2] = {0, 1};
  TEST_FAIL_IF(geteuid() == 0);
  ret = setgroups(2, list);
  TEST_FAIL_IF(ret != -1);
  TEST(errno == EPERM);
}

/**
 * @testname setgroups_2
 * @testfor setgroups
 *
 * @todo require root privilege.
 */
TEST_CASE(setgroups_2)
{
  int ret;
  TEST_FAIL_IF(geteuid() == 0);
  setgroups(0, NULL);
  TEST(1);
}
