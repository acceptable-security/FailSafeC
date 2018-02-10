/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdarg.h>

static void f(int x, ...){
  va_list ap;
  int n;
  
  va_start(ap, x);
  va_end(ap);

  va_start(ap, x);
  n = va_arg(ap, int);
  va_end(ap);
}

TEST_CASE(fsccbug_L82)
{
  f(1, 2);
  TEST(1);
}

