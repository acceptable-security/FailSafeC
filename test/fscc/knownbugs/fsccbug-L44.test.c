/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"

TEST_CASE(fsccbug_L44)
{
  unsigned char a[] = "\x1b\x8f";

  TEST_FAIL_IF(a[0] != 0x1b);
  TEST_FAIL_IF(a[1] != 0x8f);
  TEST_FAIL_IF(a[2] != 0x00);
  TEST_FAIL_IF(sizeof(a) != 3);
  TEST(1);
}

