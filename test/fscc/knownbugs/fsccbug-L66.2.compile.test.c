/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <stdio.h>

int main(int argc, char **argv){
  int i = 0;
  goto label;

  for(; i<5; i++){
    char a[10];

label:
    printf("%p\n", a);
  }
  return 0;
}
