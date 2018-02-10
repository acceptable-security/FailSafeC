/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdio.h>

static int p[2][2] = { {1}, {1} };

TEST_CASE(fsccbug_L57)
{
  TEST_FAIL_IF(p[0][0] != 1);
  TEST_FAIL_IF(p[0][1] != 0);
  TEST_FAIL_IF(p[1][0] != 1);
  TEST_FAIL_IF(p[1][1] != 0);

  TEST(1);
}
