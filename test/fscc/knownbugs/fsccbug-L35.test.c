/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <signal.h>
#include <stdio.h>
#include <unistd.h>
#include "common.h"

static volatile int run = 1;

static void handler2(int sig){
  TEST_FAIL_IF(run != 2);
  run = 0;
}

static void handler1(int sig){
  signal(SIGALRM, handler2);
  run = 0;
  alarm(2);
}

TEST_CASE(fsccbug_L35)
{
  int c;
  signal(SIGALRM, handler1);
  alarm(1);
  for(c=0, run=1; run; c++);
  for(c=0, run=2; run == 2; c++);
  TEST(1);
}
