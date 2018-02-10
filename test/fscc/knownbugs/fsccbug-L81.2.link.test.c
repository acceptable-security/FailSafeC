/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <sys/select.h>

int main(int argc, char **argv){
  fd_set s, d;
  *(&d) = s;
  return 0;
}
