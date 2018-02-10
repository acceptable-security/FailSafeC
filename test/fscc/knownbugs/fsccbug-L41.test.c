/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdio.h>

TEST_CASE(fsccbug_L41)
{
  char *a = "a", *b = "b", *tmp;
  int i;

  for(i=0; i<2; i++){
    TEST_FAIL_IF(a == b);
    printf("HEAD: %s\n", (a == b) ? "eq" : "ne");
    tmp = a; a = b; b = tmp;
    printf("TAIL: %s\n", (a == b) ? "eq" : "ne");
  }
  TEST(1);
}

