/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"

TEST_CASE(C99_6_5_2_postfix_expr_1)
{
  signed char x = -1;
  char *p = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
  TEST_FAIL_IF("abc"[1] != 'b');
  TEST_FAIL_IF(1["abc"] != 'b');
  TEST_FAIL_IF((p+2)[x] != 'B');
  TEST_FAIL_IF(x[p+2]   != 'B');
  TEST(1);
}

static int f(void)
{
  return 11;
}

TEST_CASE(C99_6_5_2_postfix_expr_2)
{
  TEST_FAIL_IF((*************f)() != 11);
  TEST(1);
}

TEST_CASE(C99_6_5_2_postfix_expr_3)
{
  int a = 12;
  int b, c, d;
  b = a++;
  c = a;
  d = a--;
  TEST(a == 12 && b == 12 && c == 13 && d == 13);
}


