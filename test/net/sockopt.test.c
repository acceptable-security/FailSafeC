/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file net/sockopt.test.c
 */
#include "common.h"
#include <sys/types.h>
#include <sys/socket.h>

/**
 * @testname setsockopt_1
 * @testfor setsockopt
 */
TEST_CASE(setsockopt_1)
{
  int val, s = socket(AF_INET, SOCK_STREAM, 0);
  TEST_FAIL_IF(s == -1);

  TEST_FAIL_IF(setsockopt(s, SOL_SOCKET, SO_KEEPALIVE, &val, sizeof(val)));
  val = 0;
  TEST_FAIL_IF(setsockopt(s, SOL_SOCKET, SO_KEEPALIVE, &val, sizeof(val)));

  TEST(1);
}

/**
 * @testname setsockopt_2
 * @testfor setsockopt
 */
TEST_CASE(setsockopt_2)
{
  int val, s = socket(AF_INET, SOCK_DGRAM, 0);
  TEST_FAIL_IF(s == -1);

  val = 1;
  TEST_FAIL_IF(setsockopt(s, SOL_SOCKET, SO_BROADCAST, &val, sizeof(val)));
  val = 0;
  TEST_FAIL_IF(setsockopt(s, SOL_SOCKET, SO_BROADCAST, &val, sizeof(val)));

  TEST(1);
}


/**
 * @testname getsockopt_1
 * @testfor getsockopt
 */
TEST_CASE(getsockopt_1)
{
  char buf[16];
  socklen_t len;
  int s = socket(AF_INET, SOCK_STREAM, 0);
  TEST_FAIL_IF(s == -1);

  len = 16;
  TEST_FAIL_IF(getsockopt(s, SOL_SOCKET, SO_TYPE, buf, &len));
  TEST_FAIL_IF(len != sizeof(int));
  TEST_FAIL_IF(*(int*)buf != SOCK_STREAM);

  len = 16;
  TEST_FAIL_IF(getsockopt(s, SOL_SOCKET, SO_ACCEPTCONN, buf, &len));
  TEST_FAIL_IF(len != sizeof(int));
  TEST_FAIL_IF(*(int*)buf != 0);

  TEST(1);
}

/**
 * @testname getsockopt_2
 * @testfor getsockopt
 */
TEST_CASE(getsockopt_2)
{
  char buf[16];
  socklen_t len;
  int s = socket(AF_INET, SOCK_DGRAM, 0);
  TEST_FAIL_IF(s == -1);

  len = 16;
  TEST_FAIL_IF(getsockopt(s, SOL_SOCKET, SO_TYPE, buf, &len));
  TEST_FAIL_IF(len != sizeof(int));
  TEST_FAIL_IF(*(int*)buf != SOCK_DGRAM);

  len = 16;
  TEST_FAIL_IF(getsockopt(s, SOL_SOCKET, SO_ACCEPTCONN, buf, &len));
  TEST_FAIL_IF(len != sizeof(int));
  TEST_FAIL_IF(*(int*)buf != 0);

  TEST(1);
}
