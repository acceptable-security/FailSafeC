/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdio.h>

TEST_CASE(fsccbug_L58)
{
  int z0, z1, t0, t1;
  int l = 1;

  z0 = 1, z1 = 1;

  while (l-- > 0){
    t0 = z0;
    t1 = z1;

    z0= t0 + t1;
    z1= t0 + t1;
  }
  TEST(z0 == 2 && z1 == 2);
}
