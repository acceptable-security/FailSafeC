/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"

struct A {
  int n;
  void *p;
  short s;
};

TEST_CASE(fsccbug_L72)
{
  TEST(sizeof(struct{ int n; void *p; short s; }) == sizeof(struct A));
}

