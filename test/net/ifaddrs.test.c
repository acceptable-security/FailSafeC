/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/net/ifaddrs.test.c
 */

#include "common.h"

#include <ifaddrs.h>
#include <string.h>
#include <sys/socket.h>
#include <arpa/inet.h>
#include <net/if.h>

/**
 * @testname getifaddrs_1
 * @testfor getifaddrs
 */
TEST_CASE(getifaddrs_1)
{
  struct ifaddrs *p;
  int lo_found = 0;

  getifaddrs(&p);

  TEST_FAIL_IF(p == NULL);
  while(p){
    if(strcmp(p->ifa_name, "lo") == 0 &&
       p->ifa_addr->sa_family == AF_INET &&
       ((struct sockaddr_in*)p->ifa_addr)->sin_addr.s_addr == htonl(0x7f000001) &&
       ((struct sockaddr_in*)p->ifa_netmask)->sin_addr.s_addr == htonl(0xff000000)){
      lo_found = 1;
    }
    p = p->ifa_next;
  }

  TEST(lo_found);
}
#include <stdio.h>
/**
 * @testname getifaddrs_2
 * @testfor getifaddrs
 */
TEST_CASE(getifaddrs_2)
{
  struct ifaddrs *p;
  sa_family_t af;
  getifaddrs(&p);

  TEST_FAIL_IF(p == NULL);
  while(p){
    if (p->ifa_addr) {
      af = p->ifa_addr->sa_family;
      TEST_FAIL_IF(p->ifa_netmask && p->ifa_netmask->sa_family != af);
      /* TEST_FAIL_IF(p->ifa_broadaddr && p->ifa_broadaddr->sa_family != af); */
      TEST_FAIL_IF(p->ifa_dstaddr && p->ifa_dstaddr->sa_family != af);
    }
    p = p->ifa_next;
  }

  TEST(1);
}

/**
 * @testname if_nametoindex_1
 * @testfor if_nametoindex
 */
TEST_CASE(if_nametoindex_1)
{
  TEST_FAIL_IF(if_nametoindex("lo") == 0);
  TEST_FAIL_IF(if_nametoindex("nosuchinterface") != 0);
  TEST(1);
}

/**
 * @testname if_nametoindex_1s
 * @testfor if_nametoindex
 */
TEST_CASE_S(if_nametoindex_1s, FSC_ABRT)
{
  if_nametoindex(NULL);
  TEST_FAILED;
}

/**
 * @testname if_indextoname_1
 * @testfor if_indextoname
 */
TEST_CASE(if_indextoname_1)
{
  char buf[IF_NAMESIZE];
  TEST_FAIL_IF(if_indextoname(if_nametoindex("lo"), buf) == NULL);
  TEST_FAIL_IF(strcmp(buf, "lo") != 0);
  TEST_FAIL_IF(if_indextoname(0, buf) != NULL);
  TEST(1);
}

/**
 * @testname if_indextoname_1s
 * @testfor if_indextoname
 */
TEST_CASE_S(if_indextoname_1s, FSC_ABRT)
{
  if_indextoname(if_nametoindex("lo"), NULL);
  TEST_FAILED;
}

/**
 * @testname if_indextoname_2s
 * @testfor if_indextoname
 */
TEST_CASE_S(if_indextoname_2s, FSC_ABRT)
{
  char buf[1];
  if_indextoname(if_nametoindex("lo"), buf);
  TEST_FAILED;
}

/**
 * @testname if_nameindex_1
 * @testfor if_nameindex
 */
TEST_CASE(if_nameindex_1)
{
  struct if_nameindex *ni;
  int i;
  ni = if_nameindex();
  TEST_FAIL_IF(ni == NULL);
  for (i = 0; ni[i].if_index != 0; ++i) {
    TEST_FAIL_IF(ni[i].if_name == NULL);
    TEST_FAIL_IF(ni[i].if_index != if_nametoindex(ni[i].if_name));
  }
  TEST_FAIL_IF(ni[i].if_name != NULL);
  TEST(1);
}
