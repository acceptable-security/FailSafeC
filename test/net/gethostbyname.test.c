/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file net/gethostbyname.test.c
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

const char *nxserver = "www.example.jp";

/**
 * @testname gethostbyname_1
 * @testfor gethostbyname
 */
TEST_CASE(gethostbyname_1)
{
  struct hostent *h = gethostbyname(server);

  TEST_FAIL_IF(h == NULL);
  TEST_FAIL_IF(strcmp(server, h->h_name));
  TEST_FAIL_IF(h->h_addrtype != AF_INET);
  TEST_FAIL_IF(h->h_length != 4);
  TEST_FAIL_IF(h->h_addr_list[0] == NULL);
  TEST_FAIL_IF(memcmp(h->h_addr_list[0], v4addr, 4));

  TEST(1);
}

/**
 * @testname gethostbyname_2
 * @testfor gethostbyname
 */
TEST_CASE(gethostbyname_2)
{
  struct hostent *h = gethostbyname(nxserver);

  TEST(h == NULL);
}
