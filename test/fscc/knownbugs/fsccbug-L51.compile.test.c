/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <stdio.h>

struct foo {
  struct bar *b;
};

struct bar {
  int x;
};

int main(int argc, char **argv){
  struct foo p;
  printf("%d\n", sizeof(*p.b));
  return 0;
}
