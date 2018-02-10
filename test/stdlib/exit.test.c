/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2007 by Lepidum Co., Ltd.
 */
/**
 * @file stdlib/exit.test.c
 */

#include <stdlib.h>

#include "common.h"

/**
 * @testname exit_1
 * @testfor exit
 */
TEST_CASE(exit_1)
{
  exit(0);
  TEST_FAIL_IF(1);
}

/**
 * @testname _Exit_1
 * @testfor _Exit
 */
TEST_CASE(_Exit_1)
{
  _Exit(0);
  TEST_FAIL_IF(1);
}
