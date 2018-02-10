/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdio.h>

static char *my_strcpy(char *dest, const char *src) {
    char *d = dest;
    while(*d++ = *src++);
    return dest;
}

TEST_CASE(fsccbug_L64)
{
  char buf[100], *str = "string to copy.";

  TEST(buf == my_strcpy(buf, str));
}
