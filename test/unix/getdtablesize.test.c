/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/getdtablesize.test.c
 */
#include "common.h"
#include <unistd.h>

/**
 * @testname getdtablesize_1
 * @testfor getdtablesize
 */
TEST_CASE(getdtablesize_1)
{
  int n = getdtablesize();
  TEST(n > 0);
}
