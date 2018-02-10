/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <stdio.h>
#include "common.h"

TEST_CASE(fsccbug_L42)
{
  int n;

  sscanf("DEADBEAF", "%x", &n);
/*  printf("%X\n", n); */
  TEST(n == 0xDEADBEAF);
}

