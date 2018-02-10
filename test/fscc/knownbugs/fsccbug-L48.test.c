/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdio.h>
#include <ctype.h>

TEST_CASE(fsccbug_L48)
{
  TEST(isascii(0x1030) == 0);
}
