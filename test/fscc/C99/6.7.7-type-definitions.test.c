/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"

typedef int t;

static t f1(t (t));
static t f1(t ((t)));
static t f1(t (((t))));

static int f1(int (*x)(int y))
{
  return x(10);
}

static t f2(t t[]);
static t f2(t *t);
static t f2(t (*t));
static t f2(t ((*t)));

static int f2(int *x)
{
  return *x + 1;
}

static t f3(t (t[]));
static t f3(t ((t[])));
static t f3(t (((t[]))));
static t f3(t (t*));
static t f3(t ((t*)));
static t f3(t (((t*))));

static int f3(int (*x)(int *y))
{
  static int a = 12;
  return x(&a) + 1;
}

static t f4(t *(t));
static t f4(t (*((t))));
static t f4(t ((*(((t))))));

static int f4(int *(*x)(int y))
{
  return *x(14) + 1;
}

static t g1(t y)
{
  return y + 1;
}

static t g3(t *y)
{
  return *y + 1;
}

static t *g4(t y)
{
  static t a;
  a = y + 1;
  return &a;
}

TEST_CASE(C99_6_7_7_type_definitions_1)
{
  t a = 12;
  TEST_FAIL_IF(f1(g1) != 11);
  TEST_FAIL_IF(f2(&a) != 13);
  TEST_FAIL_IF(f3(g3) != 14);
  TEST_FAIL_IF(f4(g4) != 16);
  TEST(1);
}

TEST_CASE(C99_6_7_7_type_definitions_2)
{
  t (t) = 10;
  TEST(t == 10);
}

