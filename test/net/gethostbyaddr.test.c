/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file net/gethostbyaddr.test.c
 */
#include "common.h"

#include <netdb.h>
#include <sys/socket.h>

#include <string.h>
#include <stdio.h>

static const char server[] = "a.dns.jp";
static const char v4addr[] = { 203, 119, 1, 1 };
static const char v6addr[] = {
  0x20, 0x01, 0x0d, 0xc4,
  0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x00,
  0x00, 0x00, 0x00, 0x01
  };

static const char v4zero[] = { 0, 0, 0, 0 };
static const char v6zero[] = { 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 };

/**
 * @testname gethostbyaddr_1
 * @testfor gethostbyaddr
 */
TEST_CASE(gethostbyaddr_1)
{
  struct hostent *h = gethostbyaddr(v4addr, 4, AF_INET);

  TEST_FAIL_IF(strcmp(server, h->h_name));
  TEST_FAIL_IF(h->h_addrtype != AF_INET);
  TEST_FAIL_IF(h->h_length != 4);
  TEST_FAIL_IF(h->h_addr_list[0] == NULL);
  TEST_FAIL_IF(memcmp(h->h_addr_list[0], v4addr, 4));

  TEST(1);
}

/**
 * @testname gethostbyaddr_2
 * @testfor gethostbyaddr
 */
TEST_CASE(gethostbyaddr_2)
{
  struct hostent *h = gethostbyaddr(v6addr, 16, AF_INET6);

  TEST_FAIL_IF(strcmp(server, h->h_name));
  TEST_FAIL_IF(h->h_addrtype != AF_INET6);
  TEST_FAIL_IF(h->h_length != 16);
  TEST_FAIL_IF(h->h_addr_list[0] == NULL);
  TEST_FAIL_IF(memcmp(h->h_addr_list[0], v6addr, 16));

  TEST(1);
}

/**
 * @testname gethostbyaddr_3
 * @testfor gethostbyaddr
 */
TEST_CASE(gethostbyaddr_3)
{
  struct hostent *h = gethostbyaddr(v4zero, 4, AF_INET);

  TEST(h == NULL);
}

/**
 * @testname gethostbyaddr_4
 * @testfor gethostbyaddr
 */
TEST_CASE(gethostbyaddr_4)
{
  struct hostent *h = gethostbyaddr(v6zero, 16, AF_INET6);

  TEST(h == NULL);
}
