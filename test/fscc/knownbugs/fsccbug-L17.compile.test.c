/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

struct S {
  union {
    long long a;
    char c;
  } u;
};

char foo(struct S *s){
  char c = s->u.c;
  return c;
}
