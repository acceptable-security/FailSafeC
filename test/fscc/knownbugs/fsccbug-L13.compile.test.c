/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

int (*foo)(int);

int (*bar(void))(int){
  return foo;
}
