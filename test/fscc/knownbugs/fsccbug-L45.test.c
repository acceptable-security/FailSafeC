/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdio.h>

TEST_CASE_S(fsccbug_L45, FSC_ABRT)
{
  char *p = "hoge";
  putchar(p[5]);
}
