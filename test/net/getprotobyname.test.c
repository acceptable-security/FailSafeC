/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file net/getprotobyname.test.c
 */
#include "common.h"
#include <netdb.h>
#include <arpa/inet.h>
#include <string.h>
#include <strings.h>

/*
 * http://www.iana.org/assignments/protocol-numbers
 */

/**
 * @testname getprotobyname_1
 * @testfor getprotobyname
 */
TEST_CASE(getprotobyname_1)
{
  struct protoent *ent = getprotobyname("tcp");
  int i = 0;

  TEST_FAIL_IF(ent == NULL);

  TEST_FAIL_IF(strcasecmp(ent->p_name, "tcp"));
  TEST_FAIL_IF(ent->p_aliases == NULL);
  TEST_FAIL_IF(ent->p_proto != 6);

  while(1){
    if(ent->p_aliases[i] == NULL){ break; }

    TEST_FAIL_IF(strcasecmp(ent->p_aliases[i], "tcp"));
    i++;
  }

  TEST(1);
}

/**
 * @testname getprotobyname_2
 * @testfor getprotobyname
 */
TEST_CASE(getprotobyname_2)
{
  struct protoent *ent = getprotobyname("udp");
  int i = 0;

  TEST_FAIL_IF(ent == NULL);

  TEST_FAIL_IF(strcasecmp(ent->p_name, "udp"));
  TEST_FAIL_IF(ent->p_aliases == NULL);
  TEST_FAIL_IF(ent->p_proto != 17);

  while(1){
    if(ent->p_aliases[i] == NULL){ break; }

    TEST_FAIL_IF(strcasecmp(ent->p_aliases[i], "udp"));
    i++;
  }
  TEST(1);
}
