/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <stdio.h>

typedef union {
  int n;
  void *p;
} U;

extern void foo(U u);

void bar(void){
  U u;
  foo(u);
}

