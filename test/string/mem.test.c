/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file string/mem.test.c
 */
#include <string.h>
#include <stdlib.h>
#include "common.h"

/**
 * @testname memchr_1
 * @testfor memchr
 */
TEST_CASE(memchr_1)
{
  unsigned char d[] = { 25, 3, 27, 127, 56 };
  TEST(memchr(d, 27, sizeof d) == d + 2);
}

/**
 * @testname memchr_2
 * @testfor memchr
 */
TEST_CASE(memchr_2)
{
  unsigned char d[] = { 25, 3, 27, 127, 56 };
  TEST(memchr(d, 56, sizeof d) == d + 4);
}

/**
 * @testname memchr_3
 * @testfor memchr
 */
TEST_CASE(memchr_3)
{
  unsigned char d[] = { 25, 3, 27, 127, 56 };
  TEST(memchr(d, 25, sizeof d) == d);
}

/**
 * @testname memchr_4
 * @testfor memchr
 */
TEST_CASE(memchr_4)
{
  unsigned char d[] = { 25, 3, 27, 127, 56 };
  TEST(memchr(d, 64, sizeof d) == 0);
}

/**
 * @testname memchr_5
 * @testfor memchr
 */
TEST_CASE(memchr_5)
{
  unsigned char d[] = { 25, -3, 27, 127, -56 };
  TEST(memchr(d, -56, sizeof d) == d + 4);
}

static struct memcpy_data {
  char b;
  int n;
  short s;
  char *p;
} data[16];

static void init_data(void)
{
  int i;
  for (i = 0; i < 16; i++) {
    data[i].b = i;
    data[i].n = i;
    data[i].s = i;
    data[i].p = malloc(1);
    TEST_FAIL_IF(data[i].p == 0);
    *data[i].p = i;
  }
}

/**
 * @testname memcpy_1
 * @testfor memcpy
 */
TEST_CASE(memcpy_1)
{
  int i;
  init_data();
  TEST_FAIL_IF(memcpy(&data[8], &data[0], 8 * sizeof(struct memcpy_data)) != &data[8]);
  for (i = 0; i < 16; i++) {
    int n = i % 8;
    TEST_FAIL_IF(data[i].b != n);
    TEST_FAIL_IF(data[i].n != n);
    TEST_FAIL_IF(data[i].s != n);
    TEST_FAIL_IF(*data[i].p != n);
  }
  TEST(1);
}

/**
 * @testname memmove_1
 * @testfor memmove
 */
TEST_CASE(memmove_1)
{
  int i;
  init_data();
  TEST_FAIL_IF(memmove(&data[4], &data[0], 12 * sizeof(struct memcpy_data)) != &data[4]);
  for (i = 0; i < 16; i++) {
    int n = i < 4 ? i: i - 4;
    TEST_FAIL_IF(data[i].b != n);
    TEST_FAIL_IF(data[i].n != n);
    TEST_FAIL_IF(data[i].s != n);
    TEST_FAIL_IF(*data[i].p != n);
  }
  TEST(1);
}

/**
 * @testname memmove_2
 * @testfor memmove
 */
TEST_CASE(memmove_2)
{
  int i;
  init_data();
  TEST_FAIL_IF(memmove(&data[0], &data[4], 12 * sizeof(struct memcpy_data)) != &data[0]);
  for (i = 0; i < 16; i++) {
    int n = i < 12 ? i + 4: i;
    TEST_FAIL_IF(data[i].b != n);
    TEST_FAIL_IF(data[i].n != n);
    TEST_FAIL_IF(data[i].s != n);
    TEST_FAIL_IF(*data[i].p != n);
  }
  TEST(1);
}

/**
 * @testname memcpy_2
 * @testfor memcpy
 */
TEST_CASE(memcpy_2)
{
  int i;
  char *p = malloc(8 * sizeof(struct memcpy_data));
  TEST_FAIL_IF(p == 0);
  init_data();
  TEST_FAIL_IF(memcpy(p, &data[0], 8 * sizeof(struct memcpy_data)) != p);
  TEST_FAIL_IF(memcpy(&data[8], p, 8 * sizeof(struct memcpy_data)) != &data[8]);
  for (i = 0; i < 16; i++) {
    int n = i % 8;
    TEST_FAIL_IF(data[i].b != n);
    TEST_FAIL_IF(data[i].n != n);
    TEST_FAIL_IF(data[i].s != n);
    TEST_FAIL_IF(*data[i].p != n);
  }

  TEST_FAIL_IF(memcpy(p, &data[13].s, sizeof(short)) != p);
  TEST(*(short*)p == 5);
}

/**
 * @testname memset_1
 * @testfor memset
 */
TEST_CASE(memset_1)
{
  int i;
  unsigned char d[5];
  TEST_FAIL_IF(memset(d, 0xA5, sizeof d) != d);
  for (i = 0; i < 5; i++) {
    TEST_FAIL_IF(d[i] != 0xA5);
  }
  TEST_FAIL_IF(memset(d, -32, sizeof d) != d);
  for (i = 0; i < 5; i++) {
    TEST_FAIL_IF(d[i] != (unsigned char)-32);
  }
  TEST(1);
}

/**
 * @testname memset_2
 * @testfor memset
 */
TEST_CASE(memset_2)
{
  int i;
  TEST_FAIL_IF(memset(&data[0], 0xE3, sizeof data) != &data[0]);
  for (i = 0; i < 16; i++) {
    TEST_FAIL_IF(data[i].b != (char)0xE3);
    TEST_FAIL_IF(data[i].n != (int)0xE3E3E3E3);
    TEST_FAIL_IF(data[i].s != (short)0xE3E3);
    TEST_FAIL_IF((int)data[i].p != (int)0xE3E3E3E3);
  }
  TEST(1);
}

/**
 * @testname memset_3
 * @testfor memset
 */
TEST_CASE(memset_3)
{
  int i;
  char *p = "hello";
  for (i = 0; i < 16; i++) {
    data[i].b = i;
    data[i].n = (int)p;
    data[i].b = i;
    data[i].p = p;
  }
  TEST_FAIL_IF(memset(&data[0], 0xE3, sizeof data) != &data[0]);
  for (i = 0; i < 16; i++) {
    TEST_FAIL_IF(data[i].b != (char)0xE3);
    TEST_FAIL_IF(data[i].n != (int)0xE3E3E3E3);
    TEST_FAIL_IF(data[i].s != (short)0xE3E3);
    TEST_FAIL_IF((int)data[i].p != (int)0xE3E3E3E3);
  }
  TEST_FAIL_IF(memset(&data[0], 0, sizeof data) != &data[0]);
  for (i = 0; i < 16; i++) {
    TEST_FAIL_IF(data[i].b != 0);
    TEST_FAIL_IF(data[i].n != 0);
    TEST_FAIL_IF(data[i].s != 0);
    TEST_FAIL_IF(data[i].p != 0);
  }
  TEST(1);
}

/**
 * @testname memset_4
 * @testfor memset
 */
TEST_CASE(memset_4)
{
  int i;
  char *p[] = {"hello", "world", 0 };
  TEST_FAIL_IF(memset(p, 0xB6, sizeof p) != p);
  for (i = 0; i < 3; i++) {
    TEST_FAIL_IF((int)p[i] != (int)0xB6B6B6B6);
  }
  TEST_FAIL_IF(memset(p, 0, sizeof p) != p);
  for (i = 0; i < 3; i++) {
    TEST_FAIL_IF(p[i] != 0);
  }
  TEST(1);
}

/**
 * @testname memset_1s
 * @testfor memset
 */
TEST_CASE_S(memset_1s, FSC_ABRT)
{
  char buf[10], *p = buf;
  memset(p - 1, 0, 10);
}

/**
 * @testname memset_2s
 * @testfor memset
 */
TEST_CASE_S(memset_2s, FSC_ABRT)
{
  char buf[10], *p = buf;
  memset(p + 1, 0, 10);
}

/**
 * @testname memcmp_1
 * @testfor memcmp
 */
TEST_CASE(memcmp_1)
{
  char *p1 = "hello world";
  char *p2 = "hello";
  char *p3 = "hello_world";
  char *p4 = "gello world";
  char *p5 = "iello world";

  TEST_FAIL_IF(memcmp(p1, p2, 5) != 0);
  TEST_FAIL_IF(memcmp(p1, p2, 6) <= 0);
  TEST_FAIL_IF(memcmp(p1, p3, 5) != 0);
  TEST_FAIL_IF(memcmp(p1, p3, 12) >= 0);
  TEST_FAIL_IF(memcmp(p1, p4, 12) <= 0);
  TEST_FAIL_IF(memcmp(p1, p5, 12) >= 0);

  TEST(1);
}

/**
 * @testname memcmp_2
 * @testfor memcmp
 */
TEST_CASE(memcmp_2)
{
  int i, j, c;
  init_data();
  for (i = 0; i < 16; ++i) {
    for (j = 0; j < 16; ++j) {
      c = memcmp(data + i, data + j, sizeof(struct memcpy_data));
      TEST_FAIL_IF(i < j && c >= 0);
      TEST_FAIL_IF(i == j && c != 0);
      TEST_FAIL_IF(i > j && c <= 0);
    }
  }
  TEST(1);
}

/**
 * @testname memcmp_1s
 * @testfor memcmp
 */
TEST_CASE_S(memcmp_1s, FSC_ABRT)
{
  memcmp(NULL, NULL, 1);
}

/**
 * @testname memcmp_2s
 * @testfor memcmp
 */
TEST_CASE_S(memcmp_2s, FSC_ABRT)
{
  char buf[10] = "", *p = buf;
  memcmp(p, p + 1, 10);
}

/**
 * @testname memcmp_3s
 * @testfor memcmp
 */
TEST_CASE_S(memcmp_3s, FSC_ABRT)
{
  char buf[10] = "", *p = buf;
  memcmp(p, p - 1, 10);
}


/**
 * @testname memccpy_1
 * @testfor memccpy
 */
TEST_CASE(memccpy_1)
{
  char buf[] = "0123456789ABCDEF";
  char *p = memccpy(buf, "XYZ0123", 'Z', sizeof buf);
  TEST_FAIL_IF(p != buf + 3);
  TEST_FAIL_IF(strcmp(buf, "XYZ3456789ABCDEF") != 0);
  p = memccpy(buf, "STUVW", 'Z', 5);
  TEST_FAIL_IF(p != NULL);
  TEST_FAIL_IF(strcmp(buf, "STUVW56789ABCDEF") != 0);
  TEST(1);
}
