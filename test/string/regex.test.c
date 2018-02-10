/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2008 by Lepidum Co., Ltd.
 */

/**
 * @file string/regex.test.c
 */
#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include <regex.h>
#include "common.h"

/**
 * @testname regex_1
 * @testfor regcomp
 * @testfor regexec
 * @testfor regerror
 * @testfor regfree
 */
TEST_CASE(regex_1)
{
  regex_t reg;
  regmatch_t match[4];
  char buf[256] = "";
  size_t s;
  int err;
  TEST_FAIL_IF(regcomp(&reg, "r\\(.*\\)r \\(ex[p-r]*es\\{1,3\\}\\)", 0) != 0);

  TEST_FAIL_IF(regexec(&reg, "regular expression", 4, match, 0) != 0);
  TEST_FAIL_IF(match[0].rm_so != 0 || match[0].rm_eo != 15);
  TEST_FAIL_IF(match[1].rm_so != 1 || match[1].rm_eo != 6);
  TEST_FAIL_IF(match[2].rm_so != 8 || match[2].rm_eo != 15);
  TEST_FAIL_IF(match[3].rm_so != -1 || match[3].rm_eo != -1);
  err = regexec(&reg, "illegal expression", 4, match, 0);
  TEST_FAIL_IF(err == 0);
  s = regerror(err, &reg, buf, 256);
  TEST_FAIL_IF(s <= 0);
  TEST_FAIL_IF(strlen(buf) + 1 != s);

  regfree(&reg);
  TEST(1);
}

/**
 * @testname regex_2
 * @testfor regcomp
 * @testfor regerror
 * @testfor regfree
 */
TEST_CASE(regex_2)
{
  regex_t reg;
  char buf[256] = "";
  size_t s;
  int err = regcomp(&reg, "\\(", 0);
  TEST_FAIL_IF(err == 0);
  s = regerror(err, &reg, buf, 256);
  TEST_FAIL_IF(s <= 0);
  TEST_FAIL_IF(strlen(buf) + 1 != s);

  regfree(&reg);
  TEST(1);
}

/**
 * @testname regcomp_1s
 * @testfor regcomp
 */
TEST_CASE_S(regcomp_1s, FSC_ABRT)
{
  regcomp(NULL, ".*", 0);
  TEST_FAILED;
}

/**
 * @testname regcomp_2s
 * @testfor regcomp
 */
TEST_CASE_S(regcomp_2s, FSC_ABRT)
{
  regex_t reg;
  regcomp(&reg, NULL, 0);
  TEST_FAILED;
}

/**
 * @testname regexec_1s
 * @testfor regexec
 */
TEST_CASE_S(regexec_1s, FSC_ABRT)
{
  regex_t reg;
  regmatch_t match[1];
  regcomp(&reg, ".*", 0);
  regexec(NULL, "string", 1, match, 0);
  TEST_FAILED;
}

/**
 * @testname regexec_2s
 * @testfor regexec
 */
TEST_CASE_S(regexec_2s, FSC_ABRT)
{
  regex_t reg;
  regmatch_t match[1];
  regcomp(&reg, ".*", 0);
  regexec(&reg, NULL, 1, match, 0);
  TEST_FAILED;
}

/**
 * @testname regexec_3s
 * @testfor regexec
 */
TEST_CASE_S(regexec_3s, FSC_ABRT)
{
  regex_t reg;
  regmatch_t match[1];
  regcomp(&reg, "\\(.*\\)", 0);
  regexec(&reg, "string", 2, match, 0);
  TEST_FAILED;
}

/**
 * @testname regexec_4s
 * @testfor regexec
 */
TEST_CASE_S(regexec_4s, FSC_ABRT)
{
  regex_t reg;
  regmatch_t match[1];
  regcomp(&reg, ".*", 0);
  regexec(&reg, "string", 1, NULL, 0);
  TEST_FAILED;
}

/**
 * @testname regerror_1s
 * @testfor regerror
 */
TEST_CASE_S(regerror_1s, FSC_ABRT)
{
  regex_t reg;
  char buf[256];
  regcomp(&reg, ".*", 0);
  regerror(REG_NOMATCH, &reg, NULL, 256);
  TEST_FAILED;
}

/**
 * @testname regfree_1s
 * @testfor regfree
 */
TEST_CASE_S(regfree_1s, FSC_ABRT)
{
  regfree(NULL);
  TEST_FAILED;
}
