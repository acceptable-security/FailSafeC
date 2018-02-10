/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <stddef.h>
#include "common.h"

TEST_CASE(C99_6_5_3_unary_expr_1)
{
  int a = 12;
  int b, c, d;
  b = ++a;
  c = a;
  d = --a;
  TEST(a == 12 && b == 13 && c == 13 && d == 12);
}

static char f(void)
{
  TEST_FAILED;
  return 0;
}

TEST_CASE(C99_6_5_3_unary_expr_2)
{
  TEST_FAIL_IF(-1["abcd"] != -'b');
  TEST_FAIL_IF(+1["abcd"] != +'b');
  TEST_FAIL_IF(sizeof f() != 1);
  TEST_FAIL_IF(sizeof 'A' != sizeof (int));
  TEST(1);
}

struct X {
  int a;
  int b;
};

TEST_CASE(C99_6_5_3_unary_expr_3)
{
  size_t n = sizeof (struct A *);
  TEST_FAIL_IF(n != sizeof (void *));
  n = sizeof (struct { int a; int b; });
  TEST_FAIL_IF(n != sizeof (struct X));
  n = sizeof (struct B { int a; int b; });
  TEST_FAIL_IF(n != sizeof (struct X));
  TEST_FAIL_IF(sizeof (struct B) != sizeof (struct X));
  {
    struct B b;
    b.a = 1;
    b.b = 2;
  }
  TEST(1);
}

TEST_CASE(C99_6_5_3_unary_expr_4)
{
  unsigned char  x = 10;
  unsigned short y = 10;
  unsigned int   z = 10;
  TEST_FAIL_IF(-x != -10);
  TEST_FAIL_IF(-y != -10);
  TEST_FAIL_IF(-z != -10);
  TEST(1);
}
