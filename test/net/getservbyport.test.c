/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2007 by Lepidum Co., Ltd.
 */

/**
 * @file net/getservbyport.test.c
 */
#include "common.h"
#include <netdb.h>
#include <arpa/inet.h>
#include <string.h>

/**
 * @testname getservbyport_1
 * @testfor getservbyport
 */
TEST_CASE(getservbyport_1)
{
  struct servent *sent = getservbyport(htons(7), "tcp");

  TEST_FAIL_IF(sent == NULL);

  TEST_FAIL_IF(strcmp(sent->s_name, "echo"));
  TEST_FAIL_IF(sent->s_aliases == NULL);
  TEST_FAIL_IF(sent->s_aliases[0] != NULL);
  TEST_FAIL_IF(ntohs(sent->s_port) != 7);
  TEST_FAIL_IF(strcmp(sent->s_proto, "tcp"));

  TEST(1);
}

/**
 * @testname getservbyport_2
 * @testfor getservbyport
 */
TEST_CASE(getservbyport_2)
{
  struct servent *sent = getservbyport(htons(9), "udp");

  TEST_FAIL_IF(sent == NULL);

  TEST_FAIL_IF(strcmp(sent->s_name, "discard"));
  TEST_FAIL_IF(sent->s_aliases == NULL);
  TEST_FAIL_IF(strcmp(sent->s_aliases[0], "sink"));
  TEST_FAIL_IF(strcmp(sent->s_aliases[1], "null"));
  TEST_FAIL_IF(sent->s_aliases[2] != NULL);
  TEST_FAIL_IF(ntohs(sent->s_port) != 9);
  TEST_FAIL_IF(strcmp(sent->s_proto, "udp"));

  TEST(1);
}

/**
 * @testname getservbyport_3
 * @testfor getservbyport
 */
TEST_CASE(getservbyport_3)
{
  struct servent *sent = getservbyport(htons(7), NULL);

  TEST_FAIL_IF(sent == NULL);

  TEST_FAIL_IF(strcmp(sent->s_name, "echo"));
  TEST_FAIL_IF(sent->s_aliases == NULL);
  TEST_FAIL_IF(sent->s_aliases[0] != NULL);
  TEST_FAIL_IF(ntohs(sent->s_port) != 7);

  TEST_FAIL_IF(strcmp(sent->s_proto, "tcp") && strcmp(sent->s_proto, "udp"));

  TEST(1);
}
