/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

struct s {
  int n;
};

void f(struct s *a, struct s *b)
{
  *a = *b;
}
