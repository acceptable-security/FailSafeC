/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/chdir.test.c
 */
#include "common.h"
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <fcntl.h>

/**
 * @testname getcwd_1
 * @testfor getcwd
 */
TEST_CASE(getcwd_1)
{
  char buf[1024];

  TEST_FAIL_IF(getcwd(buf, 1024) != buf);
  TEST(getcwd(buf, 1) == 0);
}

/**
 * @testname getcwd_s
 * @testfor getcwd
 */
TEST_CASE_S(getcwd_s, FSC_ABRT)
{
  getcwd(NULL, 10);
}

/**
 * @testname getwd_getcwd_1
 * @testfor getwd
 * @testfor getcwd
 */
TEST_CASE(getwd_getcwd_1)
{
  char buf_1[1024];
  char buf_2[1024];
  chdir("/usr/bin");
  TEST_FAIL_IF(getcwd(buf_1, 1024) != buf_1);
  TEST_FAIL_IF(getwd(buf_2) != buf_2);
  TEST(strcmp(buf_1, buf_2) == 0);
}

/**
 * @testname getwd_s
 * @testfor getwd
 */
TEST_CASE_S(getwd_s, FSC_ABRT)
{
  char buf[1];
  getwd(buf);
}

/**
 * @testname chdir_getcwd_1
 * @testfor chdir
 * @testfor getcwd
 */
TEST_CASE(chdir_getcwd_1)
{
  char buf[1024];

  TEST_FAIL_IF(chdir("/") != 0);
  TEST_FAIL_IF(getcwd(buf, 2) != buf);
  TEST_FAIL_IF(strcmp(buf, "/") != 0);
  TEST_FAIL_IF(getcwd(buf, 1) != 0);

  TEST_FAIL_IF(chdir("tmp") != 0);
  TEST_FAIL_IF(getcwd(buf, 5) != buf);
  TEST_FAIL_IF(strcmp(buf, "/tmp") != 0);
  TEST_FAIL_IF(getcwd(buf, 1) != 0);

  TEST_FAIL_IF(chdir("no such directory") != -1);
  TEST_FAIL_IF(getcwd(buf, 5) != buf);
  TEST_FAIL_IF(strcmp(buf, "/tmp") != 0);
  TEST_FAIL_IF(getcwd(buf, 1) != 0);

  TEST(1);
}

/**
 * @testname fchdir_getcwd_1
 * @testfor fchdir
 * @testfor getcwd
 */
TEST_CASE(fchdir_getcwd_1)
{
  char buf[1024];
  int fd;

  TEST_FAIL_IF((fd = open("/", O_RDONLY)) < 0);
  TEST_FAIL_IF(fchdir(fd) != 0);
  TEST_FAIL_IF(close(fd) != 0);
  TEST_FAIL_IF(getcwd(buf, 2) != buf);
  TEST_FAIL_IF(strcmp(buf, "/") != 0);
  TEST_FAIL_IF(getcwd(buf, 1) != 0);

  TEST_FAIL_IF((fd = open("tmp", O_RDONLY)) < 0);
  TEST_FAIL_IF(fchdir(fd) != 0);
  TEST_FAIL_IF(close(fd) != 0);
  TEST_FAIL_IF(getcwd(buf, 5) != buf);
  TEST_FAIL_IF(strcmp(buf, "/tmp") != 0);
  TEST_FAIL_IF(getcwd(buf, 1) != 0);

  TEST_FAIL_IF((fd = open("no such directory", O_RDONLY)) >= 0);
  TEST_FAIL_IF(fchdir(fd) == 0);
  TEST_FAIL_IF(close(fd) == 0);
  TEST_FAIL_IF(getcwd(buf, 5) != buf);
  TEST_FAIL_IF(strcmp(buf, "/tmp") != 0);
  TEST_FAIL_IF(getcwd(buf, 1) != 0);

  TEST(1);
}
