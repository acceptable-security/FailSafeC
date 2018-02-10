/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file stdlib/random.test.c
 */
#include <stdlib.h>
#include "common.h"

/**
 * @testname srandom_random_1
 * @testfor srandom
 * @testfor random
 */
TEST_CASE(srandom_random_1)
{
  long v[16];
  int i;
  for (i = 0; i < 16; i++) v[i] = random();
  srandom(1);
  for (i = 0; i < 16; i++) {
    long x = random();
    TEST_FAIL_IF(x != v[i]);
  }
  srandom(12345);
  for (i = 0; i < 16; i++) v[i] = random();
  srandom(12345);
  for (i = 0; i < 16; i++) {
    long x = random();
    TEST_FAIL_IF(x != v[i]);
  }
  TEST(1);
}


