/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

enum x { a = 10 };

void f(void) {
    enum x { b = 10 };
}
