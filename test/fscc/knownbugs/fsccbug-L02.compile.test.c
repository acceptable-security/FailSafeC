/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <stdio.h>

void foo(char *d, const char* s){
  while(*d++ = *s++);
}

int main(void){
  char a[5];
  foo(a, "hoge");
  puts(a);
  return 0;
}
