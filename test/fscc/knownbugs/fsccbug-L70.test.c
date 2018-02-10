/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <stdio.h>
#include "common.h"

TEST_CASE(fsccbug_L70)
{
  int x = 0;
  printf("%lld", (long long)x);
  TEST(1);
}
