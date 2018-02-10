/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2007 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/rand.test.c
 */
#include "common.h"
#include <stdlib.h>
#include <limits.h>
#include <string.h>

#ifndef RAND_MAX
#define RAND_MAX_NOTDEFINED
#define RAND_MAX INT_MAX
#endif

/**
 * @testname rand_srand_1
 * @testfor rand
 * @testfor srand
 */
TEST_CASE(rand_srand_1)
{
  int array1[128];
  int i;

  for (i = 0; i < 128; ++i) {
    array1[i] = rand();
    TEST_FAIL_IF(array1[i] < 0 || array1[i] > RAND_MAX);
    if (i > 0) {
      TEST_FAIL_IF(array1[i] == array1[i - 1]);
    }
  }

  srand(1);
  for (i = 0; i < 128; ++i) {
    TEST_FAIL_IF(array1[i] != rand());
  }

  srand(12345);
  for (i = 0; i < 128; ++i) {
    array1[i] = rand();
    TEST_FAIL_IF(array1[i] < 0 || array1[i] > RAND_MAX);
    if (i > 0) {
      TEST_FAIL_IF(array1[i] == array1[i - 1]);
    }
  }

  srand(12345);
  for (i = 0; i < 128; ++i) {
    TEST_FAIL_IF(array1[i] != rand());
  }

  TEST(1);
}

/**
 * @testname rand_srand_2
 * @testfor rand
 * @testfor srand
 */
TEST_CASE(rand_srand_2)
{
  int array1[128], array2[128];
  int i;
  int equals = 0;

  srand(2);
  for (i = 0; i < 128; ++i) {
    array1[i] = rand();
    TEST_FAIL_IF(array1[i] < 0 || array1[i] > RAND_MAX);
    if (i > 0) {
      TEST_FAIL_IF(array1[i] == array1[i - 1]);
    }
  }

  srand(3);
  for (i = 0; i < 128; ++i) {
    array2[i] = rand();
    TEST_FAIL_IF(array2[i] < 0 || array2[i] > RAND_MAX);
    if (array1[i] == array2[i]) {
      ++equals;
    }
    if (i > 0) {
      TEST_FAIL_IF(array2[i] == array2[i - 1]);
    }
  }

  TEST(equals < 128);
}

#ifdef RAND_MAX_NOTDEFINED
#undef RAND_MAX_NOTDEFINED
#undef RAND_MAX
#endif

/**
 * @testname lrand48_srand48_1
 * @testfor lrand48
 * @testfor srand48
 */
TEST_CASE(lrand48_srand48_1)
{
  long array1[128];
  int i;

  srand48(12345);
  for (i = 0; i < 128; ++i) {
    array1[i] = lrand48();
    TEST_FAIL_IF(array1[i] < 0);
  }

  srand48(12345);
  for (i = 0; i < 128; ++i) {
    TEST_FAIL_IF(array1[i] != lrand48());
  }

  TEST(1);
}

/**
 * @testname mrand48_seed48_1
 * @testfor mrand48
 * @testfor seed48
 */
TEST_CASE(mrand48_seed48_1)
{
  long array1[128];
  unsigned short s[3];
  int i;

  s[0] = 12; s[1] = 34; s[2] = 56;
  seed48(s);
  for (i = 0; i < 128; ++i) {
    array1[i] = mrand48();
  }

  s[0] = 12; s[1] = 34; s[2] = 56;
  seed48(s);
  for (i = 0; i < 128; ++i) {
    TEST_FAIL_IF(array1[i] != mrand48());
  }

  TEST(1);
}

/**
 * @testname drand48_lcong48_1
 * @testfor drand48
 * @testfor lcong48
 */
TEST_CASE(drand48_lcong48_1)
{
  double array1[128];
  short s[7];
  int i;

  s[0] = 12; s[1] = 34; s[2] = 56; s[3] = 78; s[4] = 90; s[5] = 13; s[6] = 24;
  lcong48(s);
  for (i = 0; i < 128; ++i) {
    array1[i] = drand48();
    TEST_FAIL_IF(array1[i] < 0 || 1 <= array1[i]);
  }

  s[0] = 12; s[1] = 34; s[2] = 56; s[3] = 78; s[4] = 90; s[5] = 13; s[6] = 24;
  lcong48(s);
  for (i = 0; i < 128; ++i) {
    TEST_FAIL_IF(array1[i] != drand48());
  }

  TEST(1);
}

/**
 * @testname nrand48_1
 * @testfor nrand48
 */
TEST_CASE(nrand48_1)
{
  long array1[128];
  unsigned short s[3];
  int i;

  s[0] = 12; s[1] = 34; s[2] = 56;
  for (i = 0; i < 128; ++i) {
    array1[i] = nrand48(s);
    TEST_FAIL_IF(array1[i] < 0);
  }

  s[0] = 12; s[1] = 34; s[2] = 56;
  for (i = 0; i < 128; ++i) {
    TEST_FAIL_IF(array1[i] != nrand48(s));
  }

  TEST(1);
}

/**
 * @testname jrand48_1
 * @testfor jrand48
 */
TEST_CASE(jrand48_1)
{
  long array1[128];
  unsigned short s[3];
  int i;

  s[0] = 12; s[1] = 34; s[2] = 56;
  for (i = 0; i < 128; ++i) {
    array1[i] = jrand48(s);
  }

  s[0] = 12; s[1] = 34; s[2] = 56;
  for (i = 0; i < 128; ++i) {
    TEST_FAIL_IF(array1[i] != jrand48(s));
  }

  TEST(1);
}

/**
 * @testname erand48_1
 * @testfor erand48
 */
TEST_CASE(erand48_1)
{
  double array1[128];
  unsigned short s[3];
  int i;

  s[0] = 12; s[1] = 34; s[2] = 56;
  for (i = 0; i < 128; ++i) {
    array1[i] = erand48(s);
    TEST_FAIL_IF(array1[i] < 0 || 1 <= array1[i]);
  }

  s[0] = 12; s[1] = 34; s[2] = 56;
  for (i = 0; i < 128; ++i) {
    TEST_FAIL_IF(array1[i] != erand48(s));
  }

  TEST(1);
}
