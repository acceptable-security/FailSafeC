/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file stdlib/qsort.test.c
 */

#include <stdlib.h>
#include <string.h>

#include "common.h"

#define N 256

static unsigned int A = 1664525U, B = 1013904223U;
static unsigned int seed = 20050118U;

static unsigned int my_rand()
{
  seed = seed * A + B;
  return seed;
}

static void my_qsort(void *base, size_t nel, size_t width,
                     int (*cmp)(const void *, const void *), void *temp)
{
  int i, j;
  for (i = nel - 1; i >= 0; i--) {
    char *bp = base;
    for (j = 0; j < i; j++, bp += width) {
      if (cmp(bp, bp + width) > 0) {
        memcpy(temp, bp, width);
        memcpy(bp, bp + width, width);
        memcpy(bp + width, temp, width);
      }
    }
  }
}

#define CODE(type) \
static type r_array_##type[N], s_array_##type[N]; \
static int cmp_##type(const void *x, const void *y) \
{ \
  type a = *(const type *)x, b = *(const type *)y; \
  if (a == b)     return 0;  \
  else if (a > b) return 1;  \
  else            return -1; \
} \
static void init_##type(void) \
{ \
  int i; \
  type tmp; \
  for (i = 0; i < N; i++) { \
    type v = ((int)my_rand()) * 1.0; \
    r_array_##type[i] = s_array_##type[i] = v; \
  } \
  my_qsort(s_array_##type, N, sizeof(type), cmp_##type, &tmp); \
} \
static int array_cmp_##type(void) \
{ \
  int i; \
  for (i = 0; i < N; i++) { \
    if (r_array_##type[i] != s_array_##type[i]) return 0; \
  } \
  return 1; \
}

typedef signed char schar;
typedef unsigned int uint;
typedef unsigned short ushort;
typedef unsigned char uchar;
typedef unsigned long ulong;
CODE(int)
CODE(short)
CODE(schar)
CODE(long)
CODE(uint)
CODE(ushort)
CODE(uchar)
CODE(ulong)
CODE(float)
CODE(double)

/**
 * @testname qsort_1
 * @testfor qsort
 */
TEST_CASE(qsort_1)
{
#define EXEC(type) init_##type(); qsort(r_array_##type, N, sizeof(type), cmp_##type); TEST_FAIL_IF(!array_cmp_##type())
  EXEC(int);
  EXEC(short);
  EXEC(schar);
  EXEC(long);
  EXEC(uint);
  EXEC(ushort);
  EXEC(uchar);
  EXEC(ulong);
  EXEC(float);
  EXEC(double);
  TEST(1);
}

struct sort_data {
  char key;
  int index;
  char *pointer;
};

static struct sort_data r_array_sort_data[N];

static int cmp_sort_data_1(const void *x, const void *y)
{
  const struct sort_data *p = x, *q = y;
  if (p->key == q->key) return 0;
  else if (p->key > q->key) return 1;
  else return -1;
}

static int cmp_sort_data_2(const void *x, const void *y)
{
  const struct sort_data *p = x, *q = y;
  if (p->index == q->index) return 0;
  else if (p->index > q->index) return 1;
  else return -1;
}

/**
 * @testname qsort_2
 * @testfor qsort
 */
TEST_CASE(qsort_2)
{
  int i;
  struct sort_data *p = &r_array_sort_data[0];
  for (i = 0; i < N; i++) {
    p[i].key = my_rand();
    p[i].index = i;
    p[i].pointer = malloc(1);
    TEST_FAIL_IF(p[i].pointer == 0);
    *p[i].pointer = 0;
  }
  qsort(p, N, sizeof(struct sort_data), cmp_sort_data_1);
  for (i = 0; i < N-1; i++) {
    TEST_FAIL_IF(p[i].key > p[i+1].key);
    TEST_FAIL_IF(*p[i].pointer != 0);
    *p[i].pointer = 1;
  }
  TEST_FAIL_IF(*p[i].pointer != 0);
  *p[i].pointer = 1;

  qsort(p, N, sizeof(struct sort_data), cmp_sort_data_2);
  for (i = 0; i < N; i++) {
    TEST_FAIL_IF(p[i].index != i);
    TEST_FAIL_IF(*p[i].pointer != 1);
    *p[i].pointer = 2;
  }
  TEST(1);
}

/**
 * @testname qsort_bsearch_1
 * @testfor qsort
 * @testfor bsearch
 */
TEST_CASE(qsort_bsearch_1)
{
#define EXEC2(type) \
  do { \
    type x, *r; \
    init_##type(); \
    x = r_array_##type[0]; \
    qsort(r_array_##type, N, sizeof(type), cmp_##type); \
    TEST_FAIL_IF(!array_cmp_##type()); \
    r = (type*)bsearch(&x, r_array_##type, N, sizeof(type), cmp_##type); \
    TEST_FAIL_IF(r == NULL); \
    TEST_FAIL_IF(*r != x); \
  } while(0)

  EXEC2(int);
  EXEC2(short);
  EXEC2(schar);
  EXEC2(long);
  EXEC2(uint);
  EXEC2(ushort);
  EXEC2(uchar);
  EXEC2(ulong);
  EXEC2(float);
  EXEC2(double);
  TEST(1);
}
