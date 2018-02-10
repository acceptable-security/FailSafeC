/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file stdlib/malloc.test.c
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <alloca.h>
#include "common.h"

/**
 * @testname malloc_1
 * @testfor malloc
 */
TEST_CASE(malloc_1)
{
  char *p = malloc(14);
  TEST_FAIL_IF(p == 0);
  TEST_FAIL_IF(strcpy(p, "hello, world!") != p);
  TEST_FAIL_IF(strcmp(p, "hello, world!") != 0);
  free(p);
  TEST(1);
}

struct x {
  struct y {
    int a;
    char b[14];
  } y;
  char c;
};

/**
 * @testname malloc_2
 * @testfor malloc
 */
TEST_CASE(malloc_2)
{
  struct x *p;
  p = malloc(sizeof(struct x));
  TEST_FAIL_IF(p == 0);
  p->y.a = 1234;
  TEST_FAIL_IF(strcpy(p->y.b, "hello, world!") != p->y.b);
  p->c = 'Z';
  TEST_FAIL_IF(p->y.a != 1234);
  TEST_FAIL_IF(strcmp(p->y.b, "hello, world!") != 0);
  TEST_FAIL_IF(p->c != 'Z');
  free(p);
  TEST(1);
}

/**
 * @testname malloc_3
 * @testfor malloc
 */
TEST_CASE(malloc_3)
{
  struct x *p;
  p = malloc(sizeof(p->y));
  TEST_FAIL_IF(p == 0);
  p->y.a = 1234;
  TEST_FAIL_IF(strcpy(p->y.b, "hello, world!") != p->y.b);
  TEST_FAIL_IF(p->y.a != 1234);
  TEST_FAIL_IF(strcmp(p->y.b, "hello, world!") != 0);
  free(p);
  TEST(1);
}

/**
 * @testname malloc_4
 * @testfor malloc
 */
TEST_CASE(malloc_4)
{
  struct x *p;
  int i;
  p = malloc(sizeof(*p) * 32);
  TEST_FAIL_IF(p == 0);

  for (i = 0; i < 32; i++) {
    p[i].y.a = 1234 + i;
    sprintf(p[i].y.b, "%d", i);
    p[i].c = 'A' + i;
  }
  for (i = 0; i < 32; i++) {
    TEST_FAIL_IF(p[i].y.a != 1234 + i);
    TEST_FAIL_IF(atoi(p[i].y.b) != i);
    TEST_FAIL_IF(p[i].c != 'A' + i);
  }
  free(p);
  TEST(1);
}

/**
 * @testname malloc_5
 * @testfor malloc
 */
TEST_CASE(malloc_5)
{
  char *p;
  p = malloc(0xFFFFFFFFU);
  TEST_FAIL_IF(p != 0);
  TEST(errno == ENOMEM);
}

/**
 * @testname realloc_1
 * @testfor realloc
 */
TEST_CASE(realloc_1)
{
  char *p = realloc(0, 14);
  TEST_FAIL_IF(p == 0);
  TEST_FAIL_IF(strcpy(p, "hello, world!") != p);
  TEST_FAIL_IF(strcmp(p, "hello, world!") != 0);
  free(p);
  TEST(1);
}

/**
 * @testname realloc_2
 * @testfor realloc
 */
TEST_CASE(realloc_2)
{
  struct x *p, *q;
  p = realloc(0, sizeof(struct x));
  TEST_FAIL_IF(p == 0);
  p->y.a = 1234;
  TEST_FAIL_IF(strcpy(p->y.b, "hello, world!") != p->y.b);
  p->c = 'Z';
  TEST_FAIL_IF(p->y.a != 1234);
  TEST_FAIL_IF(strcmp(p->y.b, "hello, world!") != 0);
  TEST_FAIL_IF(p->c != 'Z');

  q = realloc(p, sizeof(p->y));
  TEST_FAIL_IF(q == 0);
  TEST_FAIL_IF(q->y.a != 1234);
  TEST_FAIL_IF(strcmp(q->y.b, "hello, world!") != 0);
  
  free(q);
  TEST(1);
}

/**
 * @testname realloc_3
 * @testfor realloc
 */
TEST_CASE(realloc_3)
{
  struct x *p, *q;
  p = realloc(0, sizeof(p->y));
  TEST_FAIL_IF(p == 0);
  p->y.a = 1234;
  TEST_FAIL_IF(strcpy(p->y.b, "hello, world!") != p->y.b);
  TEST_FAIL_IF(p->y.a != 1234);
  TEST_FAIL_IF(strcmp(p->y.b, "hello, world!") != 0);

  q = realloc(p, sizeof(*p));
  TEST_FAIL_IF(q == 0);
  q->c = 'Z';
  TEST_FAIL_IF(q->y.a != 1234);
  TEST_FAIL_IF(strcmp(q->y.b, "hello, world!") != 0);
  TEST_FAIL_IF(q->c != 'Z');
  
  free(q);
  TEST(1);
}

/**
 * @testname realloc_4
 * @testfor realloc
 */
TEST_CASE(realloc_4)
{
  struct x *p;
  int i;
  p = realloc(0, sizeof(*p) * 32);
  TEST_FAIL_IF(p == 0);

  for (i = 0; i < 32; i++) {
    p[i].y.a = 1234 + i;
    sprintf(p[i].y.b, "%d", i);
    p[i].c = 'A' + i;
  }
  for (i = 0; i < 32; i++) {
    TEST_FAIL_IF(p[i].y.a != 1234 + i);
    TEST_FAIL_IF(atoi(p[i].y.b) != i);
    TEST_FAIL_IF(p[i].c != 'A' + i);
  }
  free(p);
  TEST(1);
}

/**
 * @testname realloc_5
 * @testfor realloc
 */
TEST_CASE(realloc_5)
{
  char *p, *q;
  p = malloc(1);
  TEST_FAIL_IF(p == 0);
  q = realloc(p, 0);
  free(q);
  TEST(1);
}

/**
 * @testname realloc_6
 * @testfor realloc
 */
TEST_CASE(realloc_6)
{
  char *p;
  p = realloc(0, 0);
  free(p);
  TEST(1);
}

/**
 * @testname realloc_7
 * @testfor realloc
 */
TEST_CASE(realloc_7)
{
  char *p;
  p = realloc(0, 0xFFFFFFFFU);
  TEST_FAIL_IF(p != 0);
  TEST(errno == ENOMEM);
}

/**
 * @testname realloc_8
 * @testfor realloc
 */
TEST_CASE(realloc_8)
{
  char *p, *q;
  p = malloc(1);
  TEST_FAIL_IF(p == 0);
  *p = 'Z';
  q = realloc(p, 0xFFFFFFFFU);
  TEST_FAIL_IF(q != 0);
  TEST_FAIL_IF(errno != ENOMEM);
  TEST_FAIL_IF(*p != 'Z');
  free(p);
  TEST(1);
}

/**
 * @testname alloca_1
 * @testfor alloca
 */
TEST_CASE(alloca_1)
{
  char *p = alloca(14);
  TEST_FAIL_IF(p == 0);
  TEST_FAIL_IF(strcpy(p, "hello, world!") != p);
  TEST_FAIL_IF(strcmp(p, "hello, world!") != 0);
  TEST(1);
}

/**
 * @testname alloca_1s
 * @testfor alloca
 */
TEST_CASE_S(alloca_1s, FSC_ABRT)
{
  char *p = alloca(14);
  free(p);
  TEST(1);
}


/**
 * @testname calloc_1
 * @testfor calloc
 */
TEST_CASE(calloc_1)
{
  char *p = calloc(42, 14);
  int i;
  for (i = 0; i < 42 * 14; i++) {
    TEST_FAIL_IF(p[i] != 0);
  }
  TEST(1);
}

