/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdio.h>

#define S "123456789"

TEST_CASE(fsccbug_L43)
{
  unsigned char a[] = S;
  printf("%d\n", sizeof(S));
  printf("%d\n", sizeof(a));

  TEST_FAIL_IF(sizeof(S) != 10);
  TEST_FAIL_IF(sizeof(a) != 10);
  TEST(1);
}

