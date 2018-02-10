/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

int f(){
  char *p = "";
  const int n = p - p;

  return n + n;
}
