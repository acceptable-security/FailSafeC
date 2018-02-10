/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/net/getaddrinfo.test.c
 */
#include <netdb.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include "common.h"

#include <string.h>
#include <stdio.h>

static const char *node = "ntp.nict.jp";
static const char *service = "ntp";
static const char addr[4][4] = {
  { 0x85, 0xf3, 0xee, 0xa3 },
  { 0x85, 0xf3, 0xee, 0xa4 },
  { 0x85, 0xf3, 0xee, 0xf3 },
  { 0x85, 0xf3, 0xee, 0xf4 }
};

/**
 * @testname getaddrinfo_1
 * @testfor getaddrinfo
 */
TEST_CASE(getaddrinfo_1)
{
  int ret;
  struct addrinfo *res;
  int a[4] = { 0, 0, 0, 0 };
  int i, j;

  ret = getaddrinfo(node, service, NULL, &res);
  TEST_FAIL_IF(ret != 0);
  while(res){
    if(res->ai_family != AF_INET){
      res = res->ai_next;
      continue;
    }
    TEST_FAIL_IF(res->ai_socktype != SOCK_DGRAM && res->ai_socktype != SOCK_STREAM);
    TEST_FAIL_IF(res->ai_addrlen <= 0);

    for(j = 0; j < 4; j++){
      struct sockaddr_in *si = (struct sockaddr_in*)res->ai_addr;
      if(memcmp(&si->sin_addr, addr[j], 4) == 0){
        a[j]++;
      }
    }
    res = res->ai_next;
  }
  TEST_FAIL_IF(res != NULL);
  TEST_FAIL_IF(a[0] != 2 || a[1] != 2 || a[2] != 2 || a[3] != 2);

  TEST(1);
}

/**
 * @testname getaddrinfo_2
 * @testfor getaddrinfo
 */
TEST_CASE(getaddrinfo_2)
{
  int ret;
  struct addrinfo *res, hint = { 0, 0, 0, 0, 0, 0, 0, 0 };
  int a[4] = { 0, 0, 0, 0 };
  int i, j;

  hint.ai_family   = AF_INET;
  hint.ai_socktype = SOCK_STREAM;

  ret = getaddrinfo(node, service, &hint, &res);
  TEST_FAIL_IF(ret != 0);
  for(i = 0; i < 4; i++){
    TEST_FAIL_IF(res->ai_family != AF_INET);
    TEST_FAIL_IF(res->ai_socktype != SOCK_STREAM);
    TEST_FAIL_IF(res->ai_addrlen <= 0);
    for(j = 0; j < 4; j++){
      struct sockaddr_in *si = (struct sockaddr_in*)res->ai_addr;
      if(memcmp(&si->sin_addr, addr[j], 4) == 0){
        a[j]++;
      }
    }
    res = res->ai_next;
  }
  TEST_FAIL_IF(res != NULL);
  TEST_FAIL_IF(a[0] != 1 || a[1] != 1 || a[2] != 1 || a[3] != 1);

  TEST(1);
}
