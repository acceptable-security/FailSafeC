/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdlib.h>

TEST_CASE(fsccbug_L67)
{
  void *p[2];
  int i;

  for(i=0; i<2; i++){
    char a[10];
    p[i] = a;
  }
  TEST(p[0] == p[1]);
}
