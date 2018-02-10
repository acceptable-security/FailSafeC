/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"

TEST_CASE(fsccbug_L76)
{
  int x = 1;

  do{
    TEST_FAIL_IF(x == 0);
    x--;
    continue;
  }while(x);

  TEST(1);
}
