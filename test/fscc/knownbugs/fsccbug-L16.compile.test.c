/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

struct S {
  long long a;
};

void foo(struct S *s){
  s->a = 0;
}
