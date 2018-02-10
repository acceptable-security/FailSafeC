/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"

struct S {
  int n;
};

struct T {
  struct S s;
};

TEST_CASE(fsccbug_L77_2)
{
  struct S s = {1};
  struct T t;

  *(struct S *)&t = s;
  TEST(t.s.n == 1);
}
