/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"

TEST_CASE(C99_5_2_2_escape)
{
  TEST_FAIL_IF('\a' != 7);
  TEST_FAIL_IF('\b' != 8);
  TEST_FAIL_IF('\f' != 12);
  TEST_FAIL_IF('\n' != 10);
  TEST_FAIL_IF('\r' != 13);
  TEST_FAIL_IF('\t' != 9);
  TEST_FAIL_IF('\v' != 11);
  TEST(1);
}
