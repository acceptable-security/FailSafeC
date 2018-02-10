/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/stdlib/uio.test.c
 */
#include "common.h"
#include <sys/uio.h>
#include <string.h>
#include <fcntl.h>
#include <unistd.h>

/**
 * @testname writev_readv_1
 * @testfor writev
 * @testfor readv
 */
TEST_CASE(writev_readv_1)
{
  struct iovec vec[3];
  char rbuf[128];
  int fd;

  fd = open(TEST_FILENAME, O_WRONLY | O_CREAT | O_EXCL, 0644);
  TEST_FAIL_IF(fd < 0);
  vec[0].iov_base = "One";
  vec[0].iov_len  = 3;
  vec[1].iov_base = "Two";
  vec[1].iov_len  = 0;
  vec[2].iov_base = "Three";
  vec[2].iov_len  = 3;

  TEST_FAIL_IF(writev(fd, vec, 3) != 6);
  TEST_FAIL_IF(close(fd) != 0);

  fd = open(TEST_FILENAME, O_RDONLY);
  TEST_FAIL_IF(fd < 0);

  vec[0].iov_base = &rbuf[0];
  vec[0].iov_len  = 5;
  vec[1].iov_base = &rbuf[5];
  vec[1].iov_len  = 7;
  TEST_FAIL_IF(readv(fd, vec, 2) != 6);
  TEST_FAIL_IF(memcmp("OneThr", rbuf, 6) !=0);

  TEST(1);
}

/**
 * @testname writev_2
 * @testfor writev
 */
TEST_CASE(writev_2)
{
  struct iovec vec[3];
  char rbuf[128];
  int fd;

  fd = open(TEST_FILENAME, O_WRONLY | O_CREAT | O_EXCL, 0644);
  TEST_FAIL_IF(fd < 0);
  vec[0].iov_base = "One";
  vec[0].iov_len  = 3;
  vec[1].iov_base = "Two";
  vec[1].iov_len  = 0;
  vec[2].iov_base = "Three";
  vec[2].iov_len  = 3;

  TEST_FAIL_IF(writev(fd, vec, -1) != -1);
  TEST_FAIL_IF(close(fd) != 0);

  fd = open(TEST_FILENAME, O_RDONLY);
  TEST_FAIL_IF(fd < 0);
  TEST_FAIL_IF(read(fd, rbuf, sizeof(rbuf)) != 0);

  TEST(1);
}

/**
 * @testname writev_3
 * @testfor writev
 */
TEST_CASE(writev_3)
{
  struct iovec vec[3];
  char rbuf[128];
  int fd;

  fd = open(TEST_FILENAME, O_WRONLY | O_CREAT | O_EXCL, 0644);
  TEST_FAIL_IF(fd < 0);
  vec[0].iov_base = "not so long";
  vec[0].iov_len  = (ssize_t)((~(size_t)0) / 2);
  vec[1].iov_base = "trailing";
  vec[1].iov_len  = 1;

  TEST_FAIL_IF(writev(fd, vec, 2) != -1);
  TEST_FAIL_IF(close(fd) != 0);

  fd = open(TEST_FILENAME, O_RDONLY);
  TEST_FAIL_IF(fd < 0);
  TEST_FAIL_IF(read(fd, rbuf, sizeof(rbuf)) != 0);

  TEST(1);
}
