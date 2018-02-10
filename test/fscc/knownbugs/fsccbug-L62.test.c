/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdlib.h>

TEST_CASE(fsccbug_L62)
{
  void *p = malloc(1);
  p = realloc(p, 2);

  TEST(1);
}
