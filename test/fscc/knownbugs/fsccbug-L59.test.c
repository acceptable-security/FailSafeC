/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdlib.h>

static char s[] = { 1, 2, 3, 4, 5, 6, 7, 8 };

TEST_CASE(fsccbug_L59)
{
  char **d = malloc(sizeof(char*)*2);
  int i,j;
 
  for(i = 0; i < 2; i++){
    d[i] = malloc(8); 
    memcpy(d[i], s, 8);
    for(j = 0; j < 8; j++){
      TEST_FAIL_IF(d[i][j] != s[j]);
    }
  }
  TEST(1);
}
