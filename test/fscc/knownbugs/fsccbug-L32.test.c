/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdlib.h>
#include <string.h>

struct s {
  int a;
};

TEST_CASE(fsccbug_L32)
{
  struct s *p;
  p = malloc(sizeof(struct s) + 1);
  p->a = 1;
  memset(p, 0, sizeof(struct s) + 1);
  TEST(1);
}

