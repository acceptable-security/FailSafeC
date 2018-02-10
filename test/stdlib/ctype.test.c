/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file stdlib/ctype.test.c
 */

#include "common.h"
#include <ctype.h>
#include <stdio.h>

/**
 * @testname isascii
 * @testfor isascii
 */
TEST_CASE(isascii)
{
  TEST(isascii(0x1030) == 0);
}

/**
 * @testname tolower_toupper
 * @testfor tolower
 * @testfor toupper
 * @testfor _tolower
 * @testfor _toupper
 */
TEST_CASE(tolower_toupper)
{
  int i;

  for (i = 0; i < 256; i++) {
    if (toupper(i) != i) {
      TEST_FAIL_IF(!islower(i));
      TEST_FAIL_IF(i < 'a' || 'z' < i);
      TEST_FAIL_IF(toupper(i) != _toupper(i));
      TEST_FAIL_IF(toupper(i) != i + 'A' - 'a');
    } else {
      TEST_FAIL_IF(islower(i));
      TEST_FAIL_IF('a' <= i && i <= 'z');
    }
    if (tolower(i) != i) {
      TEST_FAIL_IF(!isupper(i));
      TEST_FAIL_IF(i < 'A' || 'Z' < i);
      TEST_FAIL_IF(tolower(i) != _tolower(i));
      TEST_FAIL_IF(tolower(i) != i + 'a' - 'A');
    } else {
      TEST_FAIL_IF(isupper(i));
      TEST_FAIL_IF('A' <= i && i <= 'Z');
    }
  }
  TEST(1);
}

/**
 * @testname toascii
 * @testfor toascii
 */
TEST_CASE(toascii)
{
  int i;
  for (i = -1; i < 256; i++) {
    TEST_FAIL_IF(toascii(i) != (i & 0x7F));
  }
  TEST(1);
}

/**
 * @testname isblank_1
 * @testfor isblank
 */
TEST_CASE(isblank_1)
{
  int i;
  for (i = 0; i < 256; i++) {
    if (isblank(i)) {
      TEST_FAIL_IF(i != ' ' && i != '\t');
    }
  }
  TEST_FAIL_IF(isblank(EOF));
  TEST(1);
}
