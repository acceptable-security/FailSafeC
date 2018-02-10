/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file net/byteorder.test.c
 */
#include <arpa/inet.h>
#include <string.h>

#include "common.h"


/**
 * @testname htonl_1
 * @testfor htonl
 */
TEST_CASE(htonl_1)
{
  long netl = htonl(0x12345678);
  TEST(memcmp(&netl, "\x12\x34\x56\x78", sizeof netl) == 0);
}

/**
 * @testname htons_1
 * @testfor htons
 */
TEST_CASE(htons_1)
{
  short nets = htons(0x1234);
  TEST(memcmp(&nets, "\x12\x34", sizeof nets) == 0);
}

/**
 * @testname ntohl_1
 * @testfor ntohl
 */
TEST_CASE(ntohl_1)
{
  long netl;
  memcpy(&netl, "\xFE\xDC\xBA\x98", sizeof netl);
  TEST(ntohl(netl) == 0xFEDCBA98);
}

/**
 * @testname ntohs_1
 * @testfor ntohs
 */
TEST_CASE(ntohs_1)
{
  short nets;
  memcpy(&nets, "\xFE\xDC", sizeof nets);
  TEST(ntohs(nets) == 0xFEDC);
}


