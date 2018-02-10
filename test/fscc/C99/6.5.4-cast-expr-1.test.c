/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"

TEST_CASE(C99_6_5_4_cast_expr_1)
{
  int n = (enum { A = 3 }) A;
  int i;
  TEST_FAIL_IF(n != A);
  TEST_FAIL_IF(3 != A);

  for (i = 0; i != (enum b { B = 4 }) 4; i = B) {
    n++;
    TEST_FAIL_IF(B != 4);
  }
  TEST_FAIL_IF(n != 4);

  if ((enum c { C = 5 }) C) {
    n++;
  } else {
    n = -C;
  }
  TEST_FAIL_IF(n != 5);
  TEST(1);
}

TEST_CASE(C99_6_5_4_cast_expr_2)
{
  (void)1;
  TEST(1);
}

