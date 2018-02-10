/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <stdio.h>

int main(int argc, char **argv){
  goto label;
  {
    char a[10];

label:
    printf("%p\n", a);
  }
  return 0;
}

