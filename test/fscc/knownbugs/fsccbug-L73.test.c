/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"

struct bits {
  unsigned int a:1;
};

TEST_CASE(fsccbug_L73)
{
  struct bits b;
  b.a = 0;
  TEST_FAIL_IF(b.a != 0);

  b.a = 1;
  TEST_FAIL_IF(b.a != 1);

  b.a++;
  TEST_FAIL_IF(b.a != 0);

  b.a = 13;
  TEST_FAIL_IF(b.a != 1);

  TEST(1);
}
