/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

struct S {
  int x, y, z;
};

struct T {
  struct S a, b;
};

int main(int argc, char **argv){
  struct T t, *p = &t;
  p->a = p->b;
  return 0;
}
