/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file unix/access.test.c
 */
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

#include "common.h"

/**
 * @testname access_1
 * @testfor access
 */
TEST_CASE(access_1)
{
  close(open(TEST_FILENAME, O_WRONLY | O_CREAT, 0644));
  TEST_FAIL_IF(access(TEST_FILENAME, R_OK) < 0);
  TEST_FAIL_IF(access(TEST_FILENAME, R_OK | W_OK) < 0);
  TEST_FAIL_IF(access(TEST_FILENAME, R_OK | W_OK | X_OK) == 0);
  TEST_FAIL_IF(access(TEST_FILENAME, F_OK) < 0);
  TEST(1);
}

/**
 * @testname access_1s
 * @testfor access
 */
TEST_CASE_S(access_1s, FSC_ABRT)
{
  access(illegal_string, R_OK);
}

/**
 * @testname chmod_1
 * @testfor chmod
 */
TEST_CASE(chmod_1)
{
  struct stat st;
  close(open(TEST_FILENAME, O_WRONLY | O_CREAT, 0644));
  TEST_FAIL_IF(chmod(TEST_FILENAME, 04704) < 0);
  stat(TEST_FILENAME, &st);
  TEST((st.st_mode & 07777) == 04704);
}

/**
 * @testname chmod_1s
 * @testfor chmod
 */
TEST_CASE_S(chmod_1s, FSC_ABRT)
{
  chmod(illegal_string, 0644);
}

/**
 * @testname fchmod_1
 * @testfor fchmod
 */
TEST_CASE(fchmod_1)
{
  struct stat st;
  int fd;
  fd = open(TEST_FILENAME, O_WRONLY | O_CREAT, 0644);
  TEST_FAIL_IF(fchmod(fd, 04704) < 0);
  stat(TEST_FILENAME, &st);
  TEST((st.st_mode & 07777) == 04704);
  close(fd);
}

/**
 * @testname fchmod_2
 * @testfor fchmod
 */
TEST_CASE(fchmod_2)
{
  TEST(fchmod(-1, 04755) < 0);
}
