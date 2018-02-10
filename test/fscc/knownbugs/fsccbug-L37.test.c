/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <stdio.h>
#include <stdlib.h>

typedef struct
{
  unsigned int *p;
  unsigned int dummy;
} INNER;

typedef struct
{
  int dummy;
  INNER inner;
} OUTER;

static void func(OUTER *outer)
{
  INNER *inner = &outer->inner;
  if(! inner->p){
    inner->p = malloc(1);
  }

  printf("----\n");
  printf("%-16s : %p\n", "inner->p", inner->p);
  printf("%-16s : %p\n", "outer->inner.p", outer->inner.p);
  TEST_FAIL_IF(inner->p != outer->inner.p);
}

TEST_CASE(fsccbug_L37)
{
  OUTER *outer = malloc(sizeof(OUTER));
  func(outer);
  func(outer);
  free(outer);
  TEST(1);
}
