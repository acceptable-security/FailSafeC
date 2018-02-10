/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file net/getservbyname.test.c
 */
#include "common.h"
#include <netdb.h>
#include <arpa/inet.h>
#include <string.h>

/**
 * @testname getservbyname_1
 * @testfor getservbyname
 */
TEST_CASE(getservbyname_1)
{
  struct servent *ent = getservbyname("echo", "tcp");

  TEST_FAIL_IF(ent == NULL);

  TEST_FAIL_IF(strcmp(ent->s_name, "echo"));
  TEST_FAIL_IF(ent->s_aliases == NULL);
  TEST_FAIL_IF(ent->s_aliases[0] != NULL);
  TEST_FAIL_IF(ntohs(ent->s_port) != 7);
  TEST_FAIL_IF(strcmp(ent->s_proto, "tcp"));

  TEST(1);
}

/**
 * @testname getservbyname_2
 * @testfor getservbyname
 */
TEST_CASE(getservbyname_2)
{
  struct servent *ent = getservbyname("discard", "udp");

  TEST_FAIL_IF(ent == NULL);

  TEST_FAIL_IF(strcmp(ent->s_name, "discard"));
  TEST_FAIL_IF(ent->s_aliases == NULL);
  TEST_FAIL_IF(strcmp(ent->s_aliases[0], "sink"));
  TEST_FAIL_IF(strcmp(ent->s_aliases[1], "null"));
  TEST_FAIL_IF(ent->s_aliases[2] != NULL);
  TEST_FAIL_IF(ntohs(ent->s_port) != 9);
  TEST_FAIL_IF(strcmp(ent->s_proto, "udp"));

  TEST(1);
}

/**
 * @testname getservbyname_3
 * @testfor getservbyname
 */
TEST_CASE(getservbyname_3)
{
  struct servent *ent = getservbyname("echo", NULL);

  TEST_FAIL_IF(ent == NULL);

  TEST_FAIL_IF(strcmp(ent->s_name, "echo"));
  TEST_FAIL_IF(ent->s_aliases == NULL);
  TEST_FAIL_IF(ent->s_aliases[0] != NULL);
  TEST_FAIL_IF(ntohs(ent->s_port) != 7);

  TEST_FAIL_IF(strcmp(ent->s_proto, "tcp") && strcmp(ent->s_proto, "udp"));

  TEST(1);
}
