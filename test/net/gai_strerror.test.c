/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/net/gai_strerror.test.c
 */
#include "common.h"
#include <netdb.h>

static int is_string(const char *s)
{
  return s && strlen(s) > 0;
}

/**
 * @testname gai_strerror_1
 * @testfor gai_strerror
 */
TEST_CASE(gai_strerror_1)
{
  TEST_FAIL_IF(!is_string(gai_strerror(0)));
  TEST_FAIL_IF(!is_string(gai_strerror(EAI_AGAIN)));
  TEST_FAIL_IF(!is_string(gai_strerror(EAI_BADFLAGS)));
  TEST_FAIL_IF(!is_string(gai_strerror(EAI_FAIL)));
  TEST_FAIL_IF(!is_string(gai_strerror(EAI_FAMILY)));
  TEST_FAIL_IF(!is_string(gai_strerror(EAI_MEMORY)));
  TEST_FAIL_IF(!is_string(gai_strerror(EAI_NONAME)));
  TEST_FAIL_IF(!is_string(gai_strerror(EAI_SERVICE)));
  TEST_FAIL_IF(!is_string(gai_strerror(EAI_SOCKTYPE)));
  TEST_FAIL_IF(!is_string(gai_strerror(EAI_SYSTEM)));
  TEST(1);
}

/**
 * @testname gai_strerror_2
 * @testfor gai_strerror
 */
TEST_CASE(gai_strerror_2)
{
#ifdef EAI_OVERFLOW
  TEST_FAIL_IF(!is_string(gai_strerror(EAI_OVERFLOW)));
  TEST(1);
#else
  TEST(0);
#endif
}

