/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <stdarg.h>

void hoge(int x, ...){
  va_list ap;
  va_start(ap, x);
  va_end(ap);
}

int main(int argc, char **argv){
  return 0;
}
