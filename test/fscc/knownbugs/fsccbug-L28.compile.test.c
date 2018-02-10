/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

void f(void) {
  enum x { a = 10 };
  {
    enum x { b = 10 };
  }
}

enum y { a = 10 };

void g(void)
{
  enum y { b = 6 };
}

