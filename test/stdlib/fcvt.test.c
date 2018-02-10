/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file stdlib/fcvt.test.c
 */

#include <stdlib.h>
#include "common.h"


/**
 * @testname fcvt_1
 * @testfor fcvt
 */
TEST_CASE(fcvt_1)
{
  int pt = 2, sign = 5;
  char *p;

  p = fcvt(123.45678, 6, &pt, &sign);
  TEST_FAIL_IF(p == NULL);
  TEST_FAIL_IF(strcmp(p, "123456780") != 0);
  TEST_FAIL_IF(pt != 3);
  TEST_FAIL_IF(sign != 0);
  p = fcvt(-0.012345678, 4, &pt, &sign);
  TEST_FAIL_IF(p == NULL);
  TEST_FAIL_IF(strcmp(p, "123") != 0);
  TEST_FAIL_IF(pt != -1);
  TEST_FAIL_IF(sign == 0);
  TEST(1);
}

/**
 * @testname ecvt_1
 * @testfor ecvt
 */
TEST_CASE(ecvt_1)
{
  int pt, sign;
  char *p;

  p = ecvt(123.45678, 5, &pt, &sign);
  TEST_FAIL_IF(p == NULL);
  TEST_FAIL_IF(strcmp(p, "12346") != 0);
  TEST_FAIL_IF(pt != 3);
  TEST_FAIL_IF(sign != 0);
  p = ecvt(-0.012345678, 7, &pt, &sign);
  TEST_FAIL_IF(p == NULL);
  TEST_FAIL_IF(strcmp(p, "1234568") != 0);
  TEST_FAIL_IF(pt != -1);
  TEST_FAIL_IF(sign == 0);
  TEST(1);
}

/**
 * @testname gcvt_1
 * @testfor gcvt
 */
TEST_CASE(gcvt_1)
{
  char buf[16];
  char *p;

  p = gcvt(123.45678, 6, buf);
  TEST_FAIL_IF(p == NULL);
  TEST_FAIL_IF(strcmp(p, "123.457") != 0);
  p = gcvt(-1.2345678e+12, 8, buf);
  TEST_FAIL_IF(p == 0);
  TEST_FAIL_IF(strcmp(p, "-1.2345678e+12") != 0);
  TEST(1);
}

/**
 * @testname gcvt_1s
 * @testfor gcvt
 */
TEST_CASE_S(gcvt_1s, FSC_ABRT)
{
  gcvt(0.0, 1, illegal_char_ptr_1);
  TEST_FAILED;
}

/**
 * @testname gcvt_2s
 * @testfor gcvt
 */
TEST_CASE_S(gcvt_2s, FSC_ABRT)
{
  gcvt(0.0, 1, illegal_char_ptr_2);
  TEST_FAILED;
}

/**
 * @testname gcvt_3s
 * @testfor gcvt
 */
TEST_CASE_S(gcvt_3s, FSC_ABRT)
{
  gcvt(0.0, 1, illegal_char_ptr_3);
  TEST_FAILED;
}

/**
 * @testname gcvt_4s
 * @testfor gcvt
 */
TEST_CASE_S(gcvt_4s, FSC_ABRT)
{
  gcvt(0.0, 1, illegal_char_ptr_4);
  TEST_FAILED;
}

/**
 * @testname gcvt_5s
 * @testfor gcvt
 */
TEST_CASE_S(gcvt_5s, FSC_ABRT)
{
  gcvt(0.0, 1, NULL);
  TEST_FAILED;
}

/**
 * @testname gcvt_6s
 * @testfor gcvt
 */
TEST_CASE_S(gcvt_6s, FSC_ABRT)
{
  char buf[4];
  gcvt(12.3456, 6, buf);
  TEST_FAILED;
}
