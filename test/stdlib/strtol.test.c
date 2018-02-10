/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file stdlib/strtol.test.c
 */

#include "common.h"
#include <stdlib.h>
#include <limits.h>
#include <stdio.h>

/**
 * @testname strtoul_1
 * @testfor strtoul
 */
TEST_CASE(strtoul_1)
{
  char *p = "123abc", *e;
  unsigned long r;
  r = strtoul(p, &e, 10);
  TEST_FAIL_IF(r != 123);
  TEST_FAIL_IF(e - p != 3);
  TEST(1);
}

/**
 * @testname strtoul_2
 * @testfor strtoul
 */
TEST_CASE(strtoul_2)
{
  char *p = "123";
  unsigned long r;
  r = strtoul(p, NULL, 10);
  TEST(r == 123);
}

/**
 * @testname strtoul_3
 * @testfor strtoul
 */
TEST_CASE(strtoul_3)
{
  char *p = "-123";
  unsigned long r;
  r = strtoul(p, NULL, 10);
  TEST(r == (unsigned long)-123);
}

/**
 * @testname strtoul_4
 * @testfor strtoul
 */
TEST_CASE(strtoul_4)
{
  char *p = "0123";
  unsigned long r;
  r = strtoul(p, NULL, 8);
  TEST(r == 0123);
}

/**
 * @testname strtoul_5
 * @testfor strtoul
 */
TEST_CASE(strtoul_5)
{
  char *p = "0x123";
  unsigned long r;
  r = strtoul(p, NULL, 16);
  TEST(r == 0x123);
}

/**
 * @testname strtoul_6
 * @testfor strtoul
 */
TEST_CASE(strtoul_6)
{
  char *p = "-0x123";
  unsigned long r;
  r = strtoul(p, NULL, 0);
  TEST(r == -0x123);
}

/**
 * @testname strtoul_7
 * @testfor strtoul
 */
TEST_CASE(strtoul_7)
{
  char *p = "123Z", *e1 = "hoge", *e2 = e1;
  unsigned long r;
  r = strtoul(p, &e2, 36);
  TEST_FAIL_IF(r != (((1 * 36 + 2) * 36 + 3) * 36 + 35));
  TEST(1);
}

#if 0
/* if base is invalid, behavior is undefined. */
/**
 * <at>testname strtoul_8
 * <at>testfor strtoul
 */
 TEST_CASE(strtoul_8)
{
  char *p = "123Z", *e1 = "hoge", *e2 = e1;
  unsigned long r;
  r = strtoul(p, &e2, 37);
  TEST_FAIL_IF(r != 0);
  TEST(e1 == e2);
}
#endif



/**
 * @testname strtoul_1s
 * @testfor strtoul
 */
TEST_CASE_S(strtoul_1s, FSC_ABRT)
{
  unsigned long r;
  r = strtoul(illegal_char_ptr_1, NULL, 0);
}

/**
 * @testname strtoul_2s
 * @testfor strtoul
 */
TEST_CASE_S(strtoul_2s, FSC_ABRT)
{
  unsigned long r;
  r = strtoul(illegal_char_ptr_2, NULL, 0);
}

/**
 * @testname strtoul_3s
 * @testfor strtoul
 */
TEST_CASE_S(strtoul_3s, FSC_ABRT)
{
  unsigned long r;
  r = strtoul(illegal_char_ptr_3, NULL, 0);
}

/**
 * @testname strtoul_4s
 * @testfor strtoul
 */
TEST_CASE_S(strtoul_4s, FSC_ABRT)
{
  unsigned long r;
  r = strtoul(illegal_char_ptr_4, NULL, 0);
}

/**
 * @testname strtoul_5s
 * @testfor strtoul
 */
TEST_CASE_S(strtoul_5s, FSC_ABRT)
{
  unsigned long r;
  r = strtoul(illegal_string, NULL, 0);
}

/**
 * @testname strtoul_6s
 * @testfor strtoul
 */
TEST_CASE_S(strtoul_6s, FSC_ABRT)
{
  unsigned long r;
  r = strtoul(NULL, NULL, 0);
}

/**
 * @testname strtol_1
 * @testfor strtol
 */
TEST_CASE(strtol_1)
{
  char *p = "123abc", *e;
  long r;
  r = strtol(p, &e, 10);
  TEST_FAIL_IF(r != 123);
  TEST_FAIL_IF(e - p != 3);
  TEST(1);
}

/**
 * @testname strtol_2
 * @testfor strtol
 */
TEST_CASE(strtol_2)
{
  char *p = "123";
  long r;
  r = strtol(p, NULL, 10);
  TEST(r == 123);
}

/**
 * @testname strtol_3
 * @testfor strtol
 */
TEST_CASE(strtol_3)
{
  char *p = "-123";
  long r;
  r = strtol(p, NULL, 10);
  TEST(r == -123);
}

/**
 * @testname strtol_4
 * @testfor strtol
 */
TEST_CASE(strtol_4)
{
  char *p = "0123";
  long r;
  r = strtol(p, NULL, 8);
  TEST(r == 0123);
}

/**
 * @testname strtol_5
 * @testfor strtol
 */
TEST_CASE(strtol_5)
{
  char *p = "0x123";
  long r;
  r = strtol(p, NULL, 16);
  TEST(r == 0x123);
}

/**
 * @testname strtol_6
 * @testfor strtol
 */
TEST_CASE(strtol_6)
{
  char *p = "-0x123";
  long r;
  r = strtol(p, NULL, 0);
  TEST(r == -0x123);
}

/**
 * @testname strtol_7
 * @testfor strtol
 */
TEST_CASE(strtol_7)
{
  char *p = "123Z", *e1 = "hoge", *e2 = e1;
  long r;
  r = strtol(p, &e2, 36);
  TEST_FAIL_IF(r != (((1 * 36 + 2) * 36 + 3) * 36 + 35));
  TEST(1);
}

#if 0
/**
 * <at>testname strtol_8
 * <at>testfor strtol
 */
 TEST_CASE(strtol_8)
{
  char *p = "123Z", *e1 = "hoge", *e2 = e1;
  long r;
  r = strtol(p, &e2, 37);
  TEST_FAIL_IF(r != 0);
  TEST(e1 == e2);
}
#endif

/**
 * @testname strtoll_1
 * @testfor strtoll
 */
TEST_CASE(strtoll_1)
{
  char *p = "123abc", *e;
  long long r;
  r = strtoll(p, &e, 10);
  TEST_FAIL_IF(r != 123);
  TEST_FAIL_IF(e - p != 3);
  TEST(1);
}

/**
 * @testname strtoll_2
 * @testfor strtoll
 */
TEST_CASE(strtoll_2)
{
  char *p = "123abc";
  long long r;
  r = strtoll(p, NULL, 10);
  TEST_FAIL_IF(r != 123);
  TEST(1);
}

/**
 * @testname strtoll_3
 * @testfor strtoll
 */
TEST_CASE(strtoll_3)
{
  char *p = "4294967296";
  long long r;
  r = strtoll(p, NULL, 10);
  TEST_FAIL_IF(r != 4294967296LL);
  TEST(1);
}

/**
 * @testname strtoll_4
 * @testfor strtoll
 */
TEST_CASE(strtoll_4)
{
  char *p = "-4294967296";
  long long r;
  r = strtoll(p, NULL, 10);
  TEST_FAIL_IF(r != -4294967296LL);
  TEST(1);
}

/**
 * @testname strtoll_5
 * @testfor strtoll
 */
TEST_CASE(strtoll_5)
{
  char *p = "9223372036854775807";
  long long r;
  r = strtoll(p, NULL, 10);
  TEST_FAIL_IF(r != 9223372036854775807LL);
  TEST(1);
}

/**
 * @testname strtoll_6
 * @testfor strtoll
 */
TEST_CASE(strtoll_6)
{
  char *p = "-9223372036854775808";
  long long r;
  r = strtoll(p, NULL, 10);
  TEST_FAIL_IF(r != -9223372036854775807LL - 1);
  TEST(1);
}

/**
 * @testname strtoll_7
 * @testfor strtoll
 */
TEST_CASE(strtoll_7)
{
  char *p = "0x7FFFFFFFFFFFFFFF";
  long long r;
  r = strtoll(p, NULL, 0);
  TEST_FAIL_IF(r != 0x7FFFFFFFFFFFFFFFLL);
  TEST(1);
}

/**
 * @testname strtoll_8
 * @testfor strtoll
 */
TEST_CASE(strtoll_8)
{
  char *p = "-0x8000000000000000";
  long long r;
  r = strtoll(p, NULL, 0);
  TEST_FAIL_IF(r != (long long)0x8000000000000000ULL);
  TEST(1);
}


/**
 * @testname strtoll_9
 * @testfor strtoll
 */
TEST_CASE(strtoll_9)
{
  char *p = "123Z", *e1 = "hoge", *e2 = e1;
  long long r;
  r = strtoll(p, &e2, 36);
  TEST_FAIL_IF(r != (((1 * 36 + 2) * 36 + 3) * 36 + 35));
  TEST(1);
}

#if 0
/**
 * <at>testname strtoll_10
 * <at>testfor strtoll
 */
 TEST_CASE(strtoll_10)
{
  char *p = "123Z", *e1 = "hoge", *e2 = e1;
  long long r;
  r = strtoll(p, &e2, 37);
  TEST_FAIL_IF(r != 0);
  TEST(e1 == e2);
}
#endif

/**
 * @testname atoll_1
 * @testfor atoll
 */
TEST_CASE(atoll_1)
{
  char buf[1024];
  TEST_FAIL_IF(atoll("0") != 0);
  TEST_FAIL_IF(atoll("-1") != -1);
  TEST_FAIL_IF(atoll("1") != 1);
  sprintf(buf, "%lld", LLONG_MAX);
  TEST_FAIL_IF(atoll(buf) != LLONG_MAX);
  sprintf(buf, "%lld", LLONG_MIN);
  TEST_FAIL_IF(atoll(buf) != LLONG_MIN);
  TEST(1);
}

/**
 * @testname atoll_1s
 * @testfor atoll
 */
TEST_CASE_S(atoll_1s, FSC_ABRT)
{
  atoll(NULL);
}

/**
 * @testname strtoull_1
 * @testfor strtoull
 */
TEST_CASE(strtoull_1)
{
  char *p = "123abc", *e = NULL;
  unsigned long long r;
  r = strtoull(p, &e, 10);
  TEST_FAIL_IF(r != 123);
  TEST_FAIL_IF(e - p != 3);
  TEST(1);
}

/**
 * @testname strtoull_2
 * @testfor strtoull
 */
TEST_CASE(strtoull_2)
{
  char *p = "123";
  unsigned long long r;
  r = strtoull(p, NULL, 10);
  TEST_FAIL_IF(r != 123);
  TEST(1);
}

/**
 * @testname strtoull_3
 * @testfor strtoull
 */
TEST_CASE(strtoull_3)
{
  char *p = "-123";
  unsigned long long r;
  r = strtoull(p, NULL, 10);
  TEST_FAIL_IF(r != (unsigned long long)-123);
  TEST(1);
}

/**
 * @testname strtoull_4
 * @testfor strtoull
 */
TEST_CASE(strtoull_4)
{
  char *p = "4294967296";
  unsigned long long r;
  r = strtoull(p, NULL, 10);
  TEST_FAIL_IF(r != 4294967296ULL);
  TEST(1);
}

/**
 * @testname strtoull_5
 * @testfor strtoull
 */
TEST_CASE(strtoull_5)
{
  char *p = "-4294967297";
  unsigned long long r;
  r = strtoull(p, NULL, 10);
  TEST_FAIL_IF(r != (unsigned long long)-4294967297LL);
  TEST(1);
}

/**
 * @testname strtoull_6
 * @testfor strtoull
 */
TEST_CASE(strtoull_6)
{
  char *p = "18446744073709551615";
  unsigned long long r;
  r = strtoull(p, NULL, 10);
  TEST_FAIL_IF(r != 18446744073709551615ULL);
  TEST(1);
}

/**
 * @testname strtoull_7
 * @testfor strtoull
 */
TEST_CASE(strtoull_7)
{
  char *p = "0xFFFFFFFFFFFFFFFF";
  unsigned long long r;
  r = strtoull(p, NULL, 0);
  TEST_FAIL_IF(r != 0xFFFFFFFFFFFFFFFFULL);
  TEST(1);
}

/**
 * @testname strtoull_8
 * @testfor strtoull
 */
TEST_CASE(strtoull_8)
{
  char *p = "123Z";
  unsigned long long r;
  r = strtoull(p, NULL, 36);
  TEST_FAIL_IF(r != (((1 * 36 + 2) * 36 + 3) * 36 + 35));
  TEST(1);
}

/**
 * @testname strtoull_1s
 * @testfor strtoull
 */
TEST_CASE_S(strtoull_1s, FSC_ABRT)
{
  strtoull(illegal_char_ptr_1, NULL, 0);
}

/**
 * @testname strtoull_2s
 * @testfor strtoull
 */
TEST_CASE_S(strtoull_2s, FSC_ABRT)
{
  strtoull(illegal_char_ptr_2, NULL, 0);
}

/**
 * @testname strtoull_3s
 * @testfor strtoull
 */
TEST_CASE_S(strtoull_3s, FSC_ABRT)
{
  strtoull(illegal_char_ptr_3, NULL, 0);
}

/**
 * @testname strtoull_4s
 * @testfor strtoull
 */
TEST_CASE_S(strtoull_4s, FSC_ABRT)
{
  strtoull(illegal_char_ptr_4, NULL, 0);
}

/**
 * @testname strtoull_5s
 * @testfor strtoull
 */
TEST_CASE_S(strtoull_5s, FSC_ABRT)
{
  strtoull(illegal_string, NULL, 0);
}

/**
 * @testname strtoull_6s
 * @testfor strtoull
 */
TEST_CASE_S(strtoull_6s, FSC_ABRT)
{
  strtoull(NULL, NULL, 0);
}

/**
 * @testname atoi_1
 * @testfor atoi
 */
TEST_CASE(atoi_1)
{
  char buf[256];
  TEST_FAIL_IF(atoi("0") != 0);
  TEST_FAIL_IF(atoi("1") != 1);
  TEST_FAIL_IF(atoi("-1") != -1);
  sprintf(buf, "%d", INT_MAX);
  TEST_FAIL_IF(atoi(buf) != INT_MAX);
  sprintf(buf, "%d", INT_MIN);
  TEST_FAIL_IF(atoi(buf) != INT_MIN);
  TEST(1);
}

/**
 * @testname atoi_1s
 * @testfor atoi
 */
TEST_CASE_S(atoi_1s, FSC_ABRT)
{
  atoi(NULL);
}

/**
 * @testname atol_1
 * @testfor atol
 */
TEST_CASE(atol_1)
{
  char buf[256];
  TEST_FAIL_IF(atol("0") != 0);
  TEST_FAIL_IF(atol("1") != 1);
  TEST_FAIL_IF(atol("-1") != -1);
  sprintf(buf, "%ld", LONG_MAX);
  TEST_FAIL_IF(atol(buf) != LONG_MAX);
  sprintf(buf, "%ld", LONG_MIN);
  TEST_FAIL_IF(atol(buf) != LONG_MIN);
  TEST(1);
}

/**
 * @testname atol_1s
 * @testfor atol
 */
TEST_CASE_S(atol_1s, FSC_ABRT)
{
  atol(NULL);
}

/**
 * @testname strtod_1
 * @testfor strtod
 */
TEST_CASE(strtod_1)
{
  char *p = "1.23abc", *e;
  double r;
  r = strtod(p, &e);
  TEST_FAIL_IF(r != 1.23);
  TEST_FAIL_IF(e - p != 4);
  TEST(1);
}

/**
 * @testname strtod_2
 * @testfor strtod
 */
TEST_CASE(strtod_2)
{
  char *p = "-1.23abc";
  double r;
  r = strtod(p, NULL);
  TEST_FAIL_IF(r != -1.23);
  TEST(1);
}

/**
 * @testname strtod_3
 * @testfor strtod
 */
TEST_CASE(strtod_3)
{
  char *p = "+1.23";
  double r;
  r = strtod(p, NULL);
  TEST_FAIL_IF(r != 1.23);
  TEST(1);
}

/**
 * @testname strtod_4
 * @testfor strtod
 */
TEST_CASE(strtod_4)
{
  char *p = "  1.2e+12";
  double r;
  r = strtod(p, NULL);
  TEST_FAIL_IF(r != 1.2e+12);
  TEST(1);
}

/**
 * @testname strtod_5
 * @testfor strtod
 */
TEST_CASE(strtod_5)
{
  char *p = "INF";
  double r;
  r = strtod(p, NULL);
  TEST_FAIL_IF(r != r / 2);
  TEST(1);
}

/**
 * @testname strtod_6
 * @testfor strtod
 */
TEST_CASE(strtod_6)
{
  char *p = "-INF";
  double r;
  r = strtod(p, NULL);
  TEST_FAIL_IF(r != r / 2);
  TEST(1);
}

/**
 * @testname strtod_7
 * @testfor strtod
 */
TEST_CASE(strtod_7)
{
  char *p = "NAN";
  double r;
  r = strtod(p, NULL);
  TEST_FAIL_IF(r == r);
  TEST(1);
}

/**
 * @testname strtod_1s
 * @testfor strtod
 */
TEST_CASE_S(strtod_1s, FSC_ABRT)
{
  strtod(illegal_char_ptr_1, NULL);
}

/**
 * @testname strtod_2s
 * @testfor strtod
 */
TEST_CASE_S(strtod_2s, FSC_ABRT)
{
  strtod(illegal_char_ptr_2, NULL);
}

/**
 * @testname strtod_3s
 * @testfor strtod
 */
TEST_CASE_S(strtod_3s, FSC_ABRT)
{
  strtod(illegal_char_ptr_3, NULL);
}

/**
 * @testname strtod_4s
 * @testfor strtod
 */
TEST_CASE_S(strtod_4s, FSC_ABRT)
{
  strtod(illegal_char_ptr_4, NULL);
}

/**
 * @testname strtod_5s
 * @testfor strtod
 */
TEST_CASE_S(strtod_5s, FSC_ABRT)
{
  strtod(illegal_string, NULL);
}

/**
 * @testname strtod_6s
 * @testfor strtod
 */
TEST_CASE_S(strtod_6s, FSC_ABRT)
{
  strtod(NULL, NULL);
}

/**
 * @testname strtof_1
 * @testfor strtof
 */
TEST_CASE(strtof_1)
{
  char *p = "1.23abc", *e;
  float r;
  r = strtof(p, &e);
  TEST_FAIL_IF(r != 1.23f);
  TEST_FAIL_IF(e - p != 4);
  TEST(1);
}

/**
 * @testname strtof_2
 * @testfor strtof
 */
TEST_CASE(strtof_2)
{
  char *p = "-1.23abc";
  float r;
  r = strtof(p, NULL);
  TEST_FAIL_IF(r != -1.23f);
  TEST(1);
}

/**
 * @testname strtof_3
 * @testfor strtof
 */
TEST_CASE(strtof_3)
{
  char *p = "+1.23";
  float r;
  r = strtof(p, NULL);
  TEST_FAIL_IF(r != 1.23f);
  TEST(1);
}

/**
 * @testname strtof_4
 * @testfor strtof
 */
TEST_CASE(strtof_4)
{
  char *p = "  1.2e+12";
  float r;
  r = strtof(p, NULL);
  TEST_FAIL_IF(r != 1.2e+12f);
  TEST(1);
}

/**
 * @testname strtof_5
 * @testfor strtof
 */
TEST_CASE(strtof_5)
{
  char *p = "INF";
  float r;
  r = strtof(p, NULL);
  TEST_FAIL_IF(r != r / 2);
  TEST(1);
}

/**
 * @testname strtof_6
 * @testfor strtof
 */
TEST_CASE(strtof_6)
{
  char *p = "-INF";
  float r;
  r = strtof(p, NULL);
  TEST_FAIL_IF(r != r / 2);
  TEST(1);
}

/**
 * @testname strtof_7
 * @testfor strtof
 */
TEST_CASE(strtof_7)
{
  char *p = "NAN";
  float r;
  r = strtof(p, NULL);
  TEST_FAIL_IF(r == r);
  TEST(1);
}

/**
 * @testname strtof_1s
 * @testfor strtof
 */
TEST_CASE_S(strtof_1s, FSC_ABRT)
{
  strtof(illegal_char_ptr_1, NULL);
}

/**
 * @testname strtof_2s
 * @testfor strtof
 */
TEST_CASE_S(strtof_2s, FSC_ABRT)
{
  strtof(illegal_char_ptr_2, NULL);
}

/**
 * @testname strtof_3s
 * @testfor strtof
 */
TEST_CASE_S(strtof_3s, FSC_ABRT)
{
  strtof(illegal_char_ptr_3, NULL);
}

/**
 * @testname strtof_4s
 * @testfor strtof
 */
TEST_CASE_S(strtof_4s, FSC_ABRT)
{
  strtof(illegal_char_ptr_4, NULL);
}

/**
 * @testname strtof_5s
 * @testfor strtof
 */
TEST_CASE_S(strtof_5s, FSC_ABRT)
{
  strtof(illegal_string, NULL);
}

/**
 * @testname strtof_6s
 * @testfor strtof
 */
TEST_CASE_S(strtof_6s, FSC_ABRT)
{
  strtof(NULL, NULL);
}


