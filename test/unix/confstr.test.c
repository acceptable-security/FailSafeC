/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file unix/confstr.test.c
 */
#include <unistd.h>
#include <string.h>

#include "common.h"

TEST_CASE(confstr_1)
{
  char buf[1024] = "@@@@@@@@@@@@@@@@";
  size_t r;

  TEST_FAIL_IF(0 == confstr(_CS_PATH, buf, 2));
  TEST_FAIL_IF('@' == buf[0]);
  TEST_FAIL_IF('\0' != buf[1]);
  TEST_FAIL_IF('@' != buf[2]);
  TEST_FAIL_IF('@' != buf[3]);

  r = confstr(_CS_PATH, buf, sizeof(buf));
  TEST_FAIL_IF(sizeof(buf) < r);
  TEST(r == strlen(buf) + 1);
}
