/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/fcntl.test.c
 */
#include "common.h"
#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>

/**
 * @testname fcntl_F_DUPFD_1
 * @testfor fcntl
 */
TEST_CASE(fcntl_F_DUPFD_1)
{
  struct stat st1, st2;
  int fd;
  fstat(1, &st1);
  fd = fcntl(1, F_DUPFD, 0);
  TEST_FAIL_IF(fd == -1);
  TEST_FAIL_IF(fstat(fd, &st2) == -1);
  TEST(st1.st_dev == st2.st_dev && st1.st_ino == st2.st_ino);
}

/**
 * @testname fcntl_F_GETFD_F_SETFD_1
 * @testfor fcntl
 */
TEST_CASE(fcntl_F_GETFD_F_SETFD_1)
{
  TEST_FAIL_IF(fcntl(1, F_SETFD, 0) == -1);
  TEST_FAIL_IF(fcntl(1, F_GETFD) != 0);
  TEST_FAIL_IF(fcntl(1, F_SETFD, FD_CLOEXEC) == -1);
  TEST_FAIL_IF(fcntl(1, F_GETFD) != FD_CLOEXEC);
  TEST(1);
}

/**
 * @testname fcntl_F_GETFL_F_SETFL_1
 * @testfor fcntl
 */
TEST_CASE(fcntl_F_GETFL_F_SETFL_1)
{
  int fl = fcntl(1, F_GETFL);
  TEST_FAIL_IF(fl == -1);
  fl ^= O_NONBLOCK;
  TEST_FAIL_IF(fcntl(1, F_SETFL, fl) == -1);
  TEST_FAIL_IF(fcntl(1, F_GETFL) != fl);
  TEST(1);
}

/**
 * @testname fcntl_F_SETLK_F_GETLK_1
 * @testfor fcntl
 */
TEST_CASE(fcntl_F_SETLK_F_GETLK_1)
{
  char buf[256] = {0};
  struct flock fl;
  write(1, buf, sizeof buf);
  fl.l_type = F_WRLCK;
  fl.l_whence = SEEK_SET;
  fl.l_start = 0;
  fl.l_len = 0;
  TEST_FAIL_IF(fcntl(1, F_GETLK, &fl) == -1);
  TEST_FAIL_IF(fl.l_type != F_UNLCK);
  fl.l_type = F_WRLCK;
  fl.l_whence = SEEK_SET;
  fl.l_start = 0;
  fl.l_len = 0;
  TEST_FAIL_IF(fcntl(1, F_SETLK, &fl) == -1);
  TEST(1);
}
