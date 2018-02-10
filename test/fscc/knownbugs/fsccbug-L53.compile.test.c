/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#ifdef __GNUC__
#define __fsc_attribute__(x)
#endif

struct foo *func(void);

struct __fsc_attribute__((named "stdlib_foo")) foo {
  int n;
};

int main(int argc, char **argv){
  return 0;
}
