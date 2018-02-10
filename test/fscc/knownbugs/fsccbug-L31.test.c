/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <stdlib.h>
#include "common.h"

struct x {
  int a;
};

struct y {
  struct x b;
  int c;
};

TEST_CASE(fsccbug_L31)
{
  struct y *p = malloc(sizeof (struct x));
  p->b.a = 1;
  TEST(1);
}
