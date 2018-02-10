/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <stdio.h>

typedef struct {
  int n;
  void *p;
} S;

extern void foo(S s);

void bar(void){
  S s;
  foo(s);
}

