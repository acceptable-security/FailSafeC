/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

struct S {
  union {
    int a;
    int b;
  } u;
};

void *p = &((struct S*)0)->u.b;
