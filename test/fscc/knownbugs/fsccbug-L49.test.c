/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdio.h>
#include <stdlib.h>

TEST_CASE(fsccbug_L49)
{
  TEST(malloc(0xffffffffU) == NULL);
}
