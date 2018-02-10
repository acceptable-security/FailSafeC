/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <stdio.h>
#include <stdlib.h>

int main(int argc, char **argv){
  char * const p = 0;
  p = malloc(1);
  printf("%p\n", p);
  return 0;
}

