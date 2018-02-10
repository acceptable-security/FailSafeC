/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file stdlib/getenv.test.c
 */

#include <stdio.h>
#include <stdlib.h>
#include "common.h"

/**
 * @testname getenv_1
 * @testfor getenv
 */
TEST_CASE(getenv_1)
{
  TEST_FAIL_IF(getenv("TEST_ENV") == 0);
  TEST(strcmp(getenv("TEST_ENV"), "TEST") == 0);
}

/**
 * @testname getenv_2
 * @testfor getenv
 */
TEST_CASE(getenv_2)
{
  TEST(getenv("TEST_ENV2") == 0);
}


/**
 * @testname setenv_getenv_1
 * @testfor setenv
 * @testfor getenv
 */
TEST_CASE(setenv_getenv_1)
{
  TEST_FAIL_IF(strcmp(getenv("TEST_ENV"), "TEST") != 0);
  TEST_FAIL_IF(setenv("TEST_ENV", "FOO", 0) != 0);
  TEST_FAIL_IF(strcmp(getenv("TEST_ENV"), "TEST") != 0);
  TEST_FAIL_IF(setenv("TEST_ENV", "FOO", 1) != 0);
  TEST_FAIL_IF(strcmp(getenv("TEST_ENV"), "FOO") != 0);
  TEST_FAIL_IF(setenv("TEST_ENV", "LONGLONGLONGLONGLONG", 1) != 0);
  TEST_FAIL_IF(strcmp(getenv("TEST_ENV"), "LONGLONGLONGLONGLONG") != 0);
  
  TEST_FAIL_IF(setenv("TEST_ENV2", "FOO", 0) != 0);
  TEST_FAIL_IF(setenv("TEST_ENV3", "FOO", 1) != 0);
  TEST_FAIL_IF(strcmp(getenv("TEST_ENV2"), "FOO") != 0);
  TEST_FAIL_IF(strcmp(getenv("TEST_ENV3"), "FOO") != 0);
  TEST(1);
}


/**
 * @testname putenv_getenv_1
 * @testfor putenv
 * @testfor getenv
 */
TEST_CASE(putenv_getenv_1)
{
  char x[] = "TEST_ENV2=FOO";
  TEST_FAIL_IF(strcmp(getenv("TEST_ENV"), "TEST") != 0);
  TEST_FAIL_IF(putenv("TEST_ENV=BAZ") != 0);
  TEST_FAIL_IF(strcmp(getenv("TEST_ENV"), "BAZ") != 0);
  TEST_FAIL_IF(putenv(x) != 0);
  TEST_FAIL_IF(strcmp(getenv("TEST_ENV2"), "FOO") != 0);
  x[10] = 'Z';
  TEST_FAIL_IF(strcmp(getenv("TEST_ENV2"), "ZOO") != 0);
  x[8] = '3';
  TEST_FAIL_IF(strcmp(getenv("TEST_ENV3"), "ZOO") != 0);
  TEST(1);
}


/**
 * @testname unsetenv_getenv_1
 * @testfor unsetenv
 * @testfor getenv
 */
TEST_CASE(unsetenv_getenv_1)
{
  TEST_FAIL_IF(strcmp(getenv("TEST_ENV"), "TEST") != 0);
  TEST_FAIL_IF(unsetenv("TEST_ENV") != 0);
  TEST_FAIL_IF(getenv("TEST_ENV") != NULL);
  TEST_FAIL_IF(getenv("TEST_ENV2") != NULL);
  TEST_FAIL_IF(unsetenv("TEST_ENV2") != 0);
  TEST_FAIL_IF(getenv("TEST_ENV") != NULL);
  TEST(1);
}

/**
 * @testname getenv_1s
 * @testfor getenv
 */
TEST_CASE_S(getenv_1s, FSC_ABRT)
{
  getenv(0);
}

/**
 * @testname getenv_2s
 * @testfor getenv
 */
TEST_CASE_S(getenv_2s, FSC_ABRT)
{
  getenv(illegal_char_ptr_1);
}

/**
 * @testname getenv_3s
 * @testfor getenv
 */
TEST_CASE_S(getenv_3s, FSC_ABRT)
{
  getenv(illegal_char_ptr_2);
}

/**
 * @testname getenv_4s
 * @testfor getenv
 */
TEST_CASE_S(getenv_4s, FSC_ABRT)
{
  getenv(illegal_char_ptr_3);
}

/**
 * @testname getenv_5s
 * @testfor getenv
 */
TEST_CASE_S(getenv_5s, FSC_ABRT)
{
  getenv(illegal_char_ptr_4);
}

/**
 * @testname getenv_6s
 * @testfor getenv
 */
TEST_CASE_S(getenv_6s, FSC_ABRT)
{
  getenv(illegal_string);
}
