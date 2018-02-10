/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/crypt.test.c
 */
#include "common.h"
#include <unistd.h>
#include <string.h>

/*
 * $ perl -e "print crypt('JOMX-DTV', '14');"
 * 14aZB2ZZnTZt6
 */

/**
 * @testname crypt_1
 * @testfor crypt
 */
TEST_CASE(crypt_1)
{
  TEST_FAIL_IF(strcmp(crypt("JOMX-DTV", "14"), "14aZB2ZZnTZt6") != 0);
  TEST_FAIL_IF(strcmp(crypt("JOMX-DTV", "14400"), "14aZB2ZZnTZt6") != 0);
  TEST(1);
}

/**
 * @testname crypt_2
 * @testfor crypt
 */
TEST_CASE(crypt_2)
{
  TEST_FAIL_IF(strcmp(crypt("JOMX-DTV", "42"), "14aZB2ZZnTZt6") == 0);

  TEST_FAIL_IF(strcmp(crypt("JOMX-TV", "14"), "14aZB2ZZnTZt6") == 0);
  TEST_FAIL_IF(strcmp(crypt("JOKM-TV", "14"), "14aZB2ZZnTZt6") == 0);
  TEST_FAIL_IF(strcmp(crypt("JOKM-DTV", "14"), "14aZB2ZZnTZt6") == 0);

  TEST(1);
}
