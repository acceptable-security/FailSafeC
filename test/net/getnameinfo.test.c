/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/net/getnameinfo.test.c
 */
#include <sys/socket.h>
#include <netdb.h>
#include <arpa/inet.h>
#include <string.h>
#include "common.h"

#define NODENAME    "a.dns.jp"
#define V4_ADDR     0xcb770101
#define V4_ADDR_STR "203.119.1.1"

/**
 * /etc/service
 * must contains     "telnet  23/tcp"
 * must not contains "telnet  23/udp"
 */

/**
 * @testname getnameinfo_1
 * @testfor getnameinfo
 */
TEST_CASE(getnameinfo_1)
{
  struct sockaddr_in sa;
  char nodeBuf[256], servBuf[64];
  int ret;

  sa.sin_family      = AF_INET;
  sa.sin_port        = htons(23);
  sa.sin_addr.s_addr = htonl(V4_ADDR);

  ret = getnameinfo((struct sockaddr *)&sa, sizeof(sa),
                    nodeBuf, sizeof(nodeBuf),
                    servBuf, sizeof(servBuf),
                    0);

  TEST_FAIL_IF(ret != 0);
  TEST_FAIL_IF(strcmp(nodeBuf, NODENAME) != 0);
  TEST_FAIL_IF(strcmp(servBuf, "telnet") != 0);

  TEST(1);
}

/**
 * @testname getnameinfo_2
 * @testfor getnameinfo
 */
TEST_CASE(getnameinfo_2)
{
  struct sockaddr_in sa;
  char nodeBuf[256], servBuf[64];
  int ret;

  sa.sin_family      = AF_INET;
  sa.sin_port        = htons(23);
  sa.sin_addr.s_addr = htonl(0x7f000001);

  ret = getnameinfo((struct sockaddr *)&sa, sizeof(sa),
                    nodeBuf, sizeof(nodeBuf),
                    servBuf, sizeof(servBuf),
                    NI_NOFQDN);

  TEST_FAIL_IF(ret != 0);
  TEST_FAIL_IF(strcmp(nodeBuf, "localhost") != 0);
  TEST_FAIL_IF(strcmp(servBuf, "telnet") != 0);

  TEST(1);
}

/**
 * @testname getnameinfo_3
 * @testfor getnameinfo
 */
TEST_CASE(getnameinfo_3)
{
  struct sockaddr_in sa;
  char nodeBuf[256], servBuf[64];
  int ret;

  sa.sin_family      = AF_INET;
  sa.sin_port        = htons(23);
  sa.sin_addr.s_addr = htonl(V4_ADDR);

  ret = getnameinfo((struct sockaddr *)&sa, sizeof(sa),
                    nodeBuf, sizeof(nodeBuf),
                    servBuf, sizeof(servBuf),
                    NI_NUMERICHOST);

  TEST_FAIL_IF(ret != 0);
  TEST_FAIL_IF(strcmp(nodeBuf, V4_ADDR_STR) != 0);
  TEST_FAIL_IF(strcmp(servBuf, "telnet") != 0);

  TEST(1);
}

/**
 * @testname getnameinfo_4
 * @testfor getnameinfo
 */
TEST_CASE(getnameinfo_4)
{
  struct sockaddr_in sa;
  char nodeBuf[256], servBuf[64];
  int ret;

  sa.sin_family      = AF_INET;
  sa.sin_port        = htons(23);
  sa.sin_addr.s_addr = htonl(0xffffffff);

  ret = getnameinfo((struct sockaddr *)&sa, sizeof(sa),
                    nodeBuf, sizeof(nodeBuf),
                    servBuf, sizeof(servBuf),
                    NI_NAMEREQD);

  TEST(ret);
}

/**
 * @testname getnameinfo_5
 * @testfor getnameinfo
 */
TEST_CASE(getnameinfo_5)
{
  struct sockaddr_in sa;
  char nodeBuf[256], servBuf[64];
  int ret;

  sa.sin_family      = AF_INET;
  sa.sin_port        = htons(23);
  sa.sin_addr.s_addr = htonl(V4_ADDR);

  ret = getnameinfo((struct sockaddr *)&sa, sizeof(sa),
                    nodeBuf, sizeof(nodeBuf),
                    servBuf, sizeof(servBuf),
                    NI_NUMERICSERV);

  TEST_FAIL_IF(ret != 0);
  TEST_FAIL_IF(strcmp(nodeBuf, NODENAME) != 0);
  TEST_FAIL_IF(strcmp(servBuf, "23") != 0);

  TEST(1);
}

/**
 * @testname getnameinfo_6
 * @testfor getnameinfo
 */
TEST_CASE(getnameinfo_6)
{
  struct sockaddr_in sa;
  char nodeBuf[256], servBuf[64];
  int ret;

  sa.sin_family      = AF_INET;
  sa.sin_port        = htons(23);
  sa.sin_addr.s_addr = htonl(V4_ADDR);

  ret = getnameinfo((struct sockaddr *)&sa, sizeof(sa),
                    nodeBuf, sizeof(nodeBuf),
                    servBuf, sizeof(servBuf),
                    NI_DGRAM);

  TEST_FAIL_IF(ret != 0);
  TEST_FAIL_IF(strcmp(nodeBuf, NODENAME) != 0);
  TEST_FAIL_IF(strcmp(servBuf, "23") != 0);

  TEST(1);
}

/**
 * @testname getnameinfo_7
 * @testfor getnameinfo
 */
TEST_CASE(getnameinfo_7)
{
  struct sockaddr_in sa;
  char nodeBuf[256], servBuf[64];
  int ret;

  sa.sin_family      = AF_INET;
  sa.sin_port        = htons(23);
  sa.sin_addr.s_addr = htonl(V4_ADDR);

  ret = getnameinfo((struct sockaddr *)&sa, sizeof(sa),
                    NULL,    sizeof(nodeBuf),
                    servBuf, sizeof(servBuf),
                    0);
  TEST_FAIL_IF(ret != 0);
  TEST_FAIL_IF(strcmp(servBuf, "telnet") != 0);

  nodeBuf[0] = -1;
  ret = getnameinfo((struct sockaddr *)&sa, sizeof(sa),
                    nodeBuf, 0,
                    servBuf, sizeof(servBuf),
                    0);
  TEST_FAIL_IF(ret != 0);
  TEST_FAIL_IF(nodeBuf[0] != (char)-1);
  TEST_FAIL_IF(strcmp(servBuf, "telnet") != 0);

  TEST(1);
}

/**
 * @testname getnameinfo_7s
 * @testfor getnameinfo
 */
TEST_CASE_S(getnameinfo_7s, FSC_ABRT)
{
  struct sockaddr_in sa;
  char nodeBuf[256], servBuf[64];
  int ret;

  sa.sin_family      = AF_INET;
  sa.sin_port        = htons(23);
  sa.sin_addr.s_addr = htonl(V4_ADDR);

  ret = getnameinfo((struct sockaddr *)&sa, sizeof(sa),
                    (void*)42, 10,
                    servBuf,   sizeof(servBuf),
                    0);
}

/**
 * @testname getnameinfo_8
 * @testfor getnameinfo
 */
TEST_CASE(getnameinfo_8)
{
  struct sockaddr_in sa;
  char nodeBuf[256], servBuf[64];
  int ret;

  sa.sin_family      = AF_INET;
  sa.sin_port        = htons(23);
  sa.sin_addr.s_addr = htonl(V4_ADDR);

  ret = getnameinfo((struct sockaddr *)&sa, sizeof(sa),
                    nodeBuf, sizeof(nodeBuf),
                    NULL,    sizeof(servBuf),
                    0);
  TEST_FAIL_IF(ret != 0);
  TEST_FAIL_IF(strcmp(nodeBuf, NODENAME) != 0);

  servBuf[0] = -1;
  ret = getnameinfo((struct sockaddr *)&sa, sizeof(sa),
                    nodeBuf, sizeof(nodeBuf),
                    servBuf, 0,
                    0);
  TEST_FAIL_IF(ret != 0);
  TEST_FAIL_IF(strcmp(nodeBuf, NODENAME) != 0);
  TEST_FAIL_IF(servBuf[0] != (char)-1);

  TEST(1);
}

/**
 * @testname getnameinfo_8s
 * @testfor getnameinfo
 */
TEST_CASE_S(getnameinfo_8s, FSC_ABRT)
{
  struct sockaddr_in sa;
  char nodeBuf[256], servBuf[64];
  int ret;

  sa.sin_family      = AF_INET;
  sa.sin_port        = htons(23);
  sa.sin_addr.s_addr = htonl(V4_ADDR);

  ret = getnameinfo((struct sockaddr *)&sa, sizeof(sa),
                    nodeBuf,   sizeof(nodeBuf),
                    (void*)42, 10,
                    0);
}


/**
 * @testname getnameinfo_10
 * @testfor getnameinfo
 */
TEST_CASE(getnameinfo_10)
{
  struct sockaddr_in sa;
  char nodeBuf[256], servBuf[64];
  int ret;

  sa.sin_family      = AF_INET;
  sa.sin_port        = htons(23);
  sa.sin_addr.s_addr = htonl(0xffffffff);

  ret = getnameinfo((struct sockaddr *)&sa, sizeof(sa),
                    nodeBuf, 1,
                    servBuf, sizeof(servBuf),
                    0);

  TEST(ret != 0);
}
