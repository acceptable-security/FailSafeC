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
  printf("%p\n", foo);
  return 0;
}

