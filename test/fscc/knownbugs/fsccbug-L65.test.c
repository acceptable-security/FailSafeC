/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdio.h>

TEST_CASE(fsccbug_L65)
{
  TEST(0x06 == (0x0e & 0x07));
}
