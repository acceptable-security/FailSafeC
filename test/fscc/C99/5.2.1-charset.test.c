/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"

TEST_CASE(C99_5_2_1_charset)
{
  int ABCDEFGHIJKLMNOPQRSTUVWXYZ = 1;
  int abcdefghijklmnopqrstuvwxyz = 2;
  int a_0123456789 = 3;
  ~!1%2&(3)*4+5,6-7.8/9<10>11?12:13^14|15;
  0["a\n"];
  'a';
  /* SP */ /* HT */	/* VT *//* FF *//* */
  TEST(1);
}
