/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdlib.h>

struct s1 {
  int n;
};

struct s2 {
  struct s1 s;
};

void foo(struct s1 s){ return; }

TEST_CASE(fsccbug_L77_1)
{
  struct s2 *p = malloc(sizeof(struct s2));
  foo(p->s);
  TEST(1);
}
