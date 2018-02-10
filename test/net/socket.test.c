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
#include <unistd.h>

static int socket_close(int d, int t, int p)
{
  int s = socket(d, t, p);
  return s != -1 && close(s) == 0;
}

/**
 * @testname socket_1
 * @testfor socket
 */
TEST_CASE(socket_1)
{
  TEST_FAIL_IF(!socket_close(AF_INET,  SOCK_STREAM, 6));
  TEST_FAIL_IF(!socket_close(AF_INET,  SOCK_STREAM, 0));
  TEST_FAIL_IF(!socket_close(AF_INET,  SOCK_DGRAM, 17));
  TEST_FAIL_IF(!socket_close(AF_INET,  SOCK_DGRAM,  0));
  TEST_FAIL_IF(!socket_close(AF_UNIX,  SOCK_STREAM, 0));
  TEST_FAIL_IF(!socket_close(AF_UNIX,  SOCK_DGRAM,  0));
  TEST(1);
}

/**
 * @testname socket_IPv6_1
 * @testfor socket
 */
TEST_CASE(socket_IPv6)
{
  TEST_FAIL_IF(!socket_close(AF_INET6, SOCK_STREAM, 6));
  TEST_FAIL_IF(!socket_close(AF_INET6, SOCK_STREAM, 0));
  TEST_FAIL_IF(!socket_close(AF_INET6, SOCK_DGRAM, 17));
  TEST_FAIL_IF(!socket_close(AF_INET6, SOCK_DGRAM,  0));
  TEST(1);
}

static int socketpair_close(int d, int t, int p)
{
  int sv[2] = {0, 0};
  if (socketpair(d, t, p, sv) == -1) {
    return 0;
  }
  return close(sv[0]) == 0 && close(sv[1]) == 0;
}

/**
 * @testname socketpair_1
 * @testfor socketpair
 */
TEST_CASE(socketpair_1)
{
  TEST_FAIL_IF(socketpair_close(AF_INET, SOCK_STREAM, 6));
  TEST_FAIL_IF(socketpair_close(AF_INET, SOCK_STREAM, 0));
  TEST_FAIL_IF(socketpair_close(AF_INET, SOCK_DGRAM, 17));
  TEST_FAIL_IF(socketpair_close(AF_INET, SOCK_DGRAM,  0));
  TEST_FAIL_IF(!socketpair_close(AF_UNIX, SOCK_STREAM, 0));
  TEST_FAIL_IF(!socketpair_close(AF_UNIX, SOCK_DGRAM, 0));
  TEST(1);
}
