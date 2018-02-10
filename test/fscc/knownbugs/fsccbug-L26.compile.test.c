/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

struct S {
  union {
    int n;
    char c[1];
  } u;
};

void f(struct S *s){
  char *p = s->u.c;
}
