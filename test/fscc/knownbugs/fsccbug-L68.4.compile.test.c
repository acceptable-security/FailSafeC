/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <stdio.h>

typedef struct {
  int n;
} S;

void foo(S s){
}

int main(int argc, char **argv){
  void (*p)(S) = foo;
  S s;
  p(s);
  return 0;
}

