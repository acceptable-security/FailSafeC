/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2007 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/hostid.test.c
 */
#include "common.h"
#include <unistd.h>

/**
 * @testname gethostid_1
 * @testfor gethostid
 */
TEST_CASE(gethostid_1)
{
  long v[16];
  int i;
  for (i = 0; i < 16; ++i) v[i] = gethostid();
  for (i = 1; i < 16; ++i) {
    TEST_FAIL_IF(v[0] != v[i]);
  }
  TEST(1);
}

