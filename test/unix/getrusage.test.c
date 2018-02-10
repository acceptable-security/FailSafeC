/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2008 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/getrusage.test.c
 */
#include "common.h"
#include <sys/time.h>
#include <sys/resource.h>

/**
 * @testname getrusage_1
 * @testfor getrusage
 */
TEST_CASE(getrusage_1)
{
  struct rusage usage;
  TEST_FAIL_IF(getrusage(RUSAGE_SELF, &usage) != 0);
  TEST_FAIL_IF(getrusage(RUSAGE_CHILDREN, &usage) != 0);
  TEST_FAIL_IF(getrusage(31415, &usage) == 0);
  TEST(1);
}

/**
 * @testname getrusage_1s
 * @testfor getrusage
 */
TEST_CASE_S(getrusage_1s, FSC_ABRT)
{
   getrusage(RUSAGE_SELF, NULL);
   TEST_FAILED;
}
