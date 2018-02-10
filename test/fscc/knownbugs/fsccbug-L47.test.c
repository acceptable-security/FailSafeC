/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdio.h>

static char *foo(){
  char a[] = "hello.";
  return a;
}

TEST_CASE_S(fsccbug_L47, FSC_ABRT)
{
  puts(foo());
}
