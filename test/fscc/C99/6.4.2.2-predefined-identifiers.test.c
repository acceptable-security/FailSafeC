/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <string.h>
#include "common.h"

static int my_func(void)
{
  return strcmp(__func__, "my_func");
}

TEST_CASE(C99_6_4_2_2_predefined_identifiers)
{
  TEST(my_func() == 0);
}

