/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file net/inet.test.c
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "common.h"
#include <string.h>

/**
 * @testname inet_addr_1
 * @testfor inet_addr
 */
TEST_CASE(inet_addr_1)
{
  in_addr_t t = -1;
  in_addr_t v = inet_addr("");
  TEST(t == v);
}

/**
 * @testname inet_addr_2
 * @testfor htonl
 * @testfor inet_addr
 */
TEST_CASE(inet_addr_2)
{
  in_addr_t t = htonl(0x7f000001);
  in_addr_t v = inet_addr("127.0.0.1");
  TEST(t == v);
}

/**
 * @testname inet_addr_3
 * @testfor inet_addr
 */
TEST_CASE(inet_addr_3)
{
  in_addr_t t = -1;
  in_addr_t v = inet_addr("0xFF.0xFF.0xFF.0xFF");
  TEST(t == v);
}

/**
 * @testname inet_ntoa_1
 * @testfor htonl
 * @testfor inet_ntoa
 */
TEST_CASE(inet_ntoa_1)
{
  struct in_addr t = { htonl(0x00000000) };
  char *p = inet_ntoa(t);
  TEST(strcmp(p, "0.0.0.0") == 0);
}

/**
 * @testname inet_ntoa_2
 * @testfor htonl
 * @testfor inet_ntoa
 */
TEST_CASE(inet_ntoa_2)
{
  struct in_addr t = { htonl(0x7f000001) };
  char *p = inet_ntoa(t);
  TEST(strcmp(p, "127.0.0.1") == 0);
}

/**
 * @testname inet_ntoa_3
 * @testfor htonl
 * @testfor inet_ntoa
 */
TEST_CASE(inet_ntoa_3)
{
  struct in_addr t = { htonl(0xffffffff) };
  char *p = inet_ntoa(t);
  TEST(strcmp(p, "255.255.255.255") == 0);
}

/**
 * @testname inet_ntop_v4
 * @testfor htonl
 * @testfor inet_ntop
 */
TEST_CASE(inet_ntop_v4)
{
  struct in_addr t;
  char buf[INET_ADDRSTRLEN];
  t.s_addr = htonl(0x00000000);
  TEST_FAIL_IF(inet_ntop(AF_INET, &t, buf, INET_ADDRSTRLEN) == NULL);
  TEST_FAIL_IF(strcmp("0.0.0.0", buf) != 0);
  t.s_addr = htonl(0x7f000001);
  TEST_FAIL_IF(inet_ntop(AF_INET, &t, buf, INET_ADDRSTRLEN) == NULL);
  TEST_FAIL_IF(strcmp("127.0.0.1", buf) != 0);
  t.s_addr = htonl(0xffffffff);
  TEST_FAIL_IF(inet_ntop(AF_INET, &t, buf, INET_ADDRSTRLEN) == NULL);
  TEST_FAIL_IF(strcmp("255.255.255.255", buf) != 0);
  TEST(1);
}

/**
 * @testname inet_pton_v4
 * @testfor htonl
 * @testfor inet_pton
 */
TEST_CASE(inet_pton_v4)
{
  struct in_addr t;
  TEST_FAIL_IF(inet_pton(AF_INET, "0.0.0.0", &t) <= 0);
  TEST_FAIL_IF(t.s_addr != htonl(0x00000000));
  TEST_FAIL_IF(inet_pton(AF_INET, "127.0.0.1", &t) <= 0);
  TEST_FAIL_IF(t.s_addr != htonl(0x7f000001));
  TEST_FAIL_IF(inet_pton(AF_INET, "255.255.255.255", &t) <= 0);
  TEST_FAIL_IF(t.s_addr != htonl(0xffffffff));
  TEST(inet_pton(AF_INET, "::1", &t) == 0);
}

static struct in6_addr loopback = {{ 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1 }};
static struct in6_addr linklocal = {{ 0xfe, 0x80, 0, 0, 0, 0, 0, 0, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77, 0x88 }};
static struct in6_addr _6to4 = {{ 0x20, 0x02, 11, 33, 55, 77, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0x1 }};

/**
 * @testname inet_ntop_v6
 * @testfor inet_ntop
 */
TEST_CASE(inet_ntop_v6)
{
  char buf[INET6_ADDRSTRLEN];

  TEST_FAIL_IF(inet_ntop(AF_INET6, &loopback, buf, INET6_ADDRSTRLEN) == NULL);
  TEST_FAIL_IF(strcmp("::1", buf) != 0);

  TEST_FAIL_IF(inet_ntop(AF_INET6, &linklocal, buf, INET6_ADDRSTRLEN) == NULL);
  TEST_FAIL_IF(strcmp("fe80::1122:3344:5566:7788", buf) != 0);

  TEST_FAIL_IF(inet_ntop(AF_INET6, &_6to4, buf, INET6_ADDRSTRLEN) == NULL);
  TEST_FAIL_IF(strcmp("2002:b21:374d:1::1", buf) != 0);

  TEST(1);
}

/**
 * @testname inet_pton_v6
 * @testfor inet_pton
 */
TEST_CASE(inet_pton_v6)
{
  struct in6_addr addr;

  TEST_FAIL_IF(inet_pton(AF_INET6, "::1", &addr) <= 0);
  TEST_FAIL_IF(memcmp(&loopback, &addr, sizeof(addr)));

  TEST_FAIL_IF(inet_pton(AF_INET6, "fe80::1122:3344:5566:7788", &addr) <= 0);
  TEST_FAIL_IF(memcmp(&linklocal, &addr, sizeof(addr)));

  TEST_FAIL_IF(inet_pton(AF_INET6, "2002:b21:374d:1::1", &addr) <= 0);
  TEST_FAIL_IF(memcmp(&_6to4, &addr, sizeof(addr)));

  TEST(1);
}
