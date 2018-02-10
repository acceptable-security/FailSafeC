/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

struct foo {
  int x[1024];
  char y[1024];
};

struct bar { 
  int x;
  struct foo y[1024];
};

int main(int argc, char **argv){
  int a = 0;
  
  return ((struct bar *)&a)->x;
}
