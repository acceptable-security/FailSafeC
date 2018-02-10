/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

typedef int t;

int f(int (t));
int f(int (*)(t));

int main(void)
{
  int (t) = 10;
  return 0;
}

