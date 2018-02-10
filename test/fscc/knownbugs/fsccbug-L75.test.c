/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"

TEST_CASE(fsccbug_L75)
{
  int x = { 123 };
  TEST(x == 123);
}
