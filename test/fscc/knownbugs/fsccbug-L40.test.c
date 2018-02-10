/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"

static int x;

static int *p() { return &x; }

TEST_CASE(fsccbug_L40)
{
  int f = !p();
  TEST(f == 0);
}

