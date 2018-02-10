/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdlib.h>

TEST_CASE_S(fsccbug_L33, FSC_ABRT)
{
  char *p = malloc(1);
  p[100] = 0;
  TEST_FAILED;
}

