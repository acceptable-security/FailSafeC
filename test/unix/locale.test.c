/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file unix/locale.test.c
 */
#define ISOC99_SOURCE
#include <locale.h>
#include <string.h>
#include <stdio.h>
#include <limits.h>
#include "common.h"

/**
 * @testname setlocale_1
 * @testfor setlocale
 */
TEST_CASE(setlocale_1)
{
  char *r;

  setlocale(LC_ALL, ""); /* environ set locale to C */
  r = setlocale(LC_ALL, NULL);
  TEST_FAIL_IF(r == NULL);
  TEST_FAIL_IF(strcmp(r, "C") != 0 && strcmp(r, "POSIX") != 0);
  TEST_FAIL_IF(strcmp(setlocale(LC_ALL, NULL), r) != 0);
  TEST_FAIL_IF(strcmp(setlocale(LC_COLLATE, NULL), r) != 0);
  TEST_FAIL_IF(strcmp(setlocale(LC_CTYPE, NULL), r) != 0);
  TEST_FAIL_IF(strcmp(setlocale(LC_MESSAGES, NULL), r) != 0);
  TEST_FAIL_IF(strcmp(setlocale(LC_MONETARY, NULL), r) != 0);
  TEST_FAIL_IF(strcmp(setlocale(LC_NUMERIC, NULL), r) != 0);
  TEST_FAIL_IF(strcmp(setlocale(LC_TIME, NULL), r) != 0);

  setlocale(LC_ALL, "POSIX");
  r = setlocale(LC_ALL, NULL);
  TEST_FAIL_IF(r == NULL);
  TEST_FAIL_IF(strcmp(r, "C") != 0 && strcmp(r, "POSIX") != 0); /* POSIX may be an alias to C */
  TEST_FAIL_IF(strcmp(setlocale(LC_ALL, NULL), r) != 0);
  TEST_FAIL_IF(strcmp(setlocale(LC_COLLATE, NULL), r) != 0);
  TEST_FAIL_IF(strcmp(setlocale(LC_CTYPE, NULL), r) != 0);
  TEST_FAIL_IF(strcmp(setlocale(LC_MESSAGES, NULL), r) != 0);
  TEST_FAIL_IF(strcmp(setlocale(LC_MONETARY, NULL), r) != 0);
  TEST_FAIL_IF(strcmp(setlocale(LC_NUMERIC, NULL), r) != 0);
  TEST_FAIL_IF(strcmp(setlocale(LC_TIME, NULL), r) != 0);
  TEST(1);
}

/**
 * @testname localeconv_1
 * @testfor localeconv
 */
TEST_CASE(localeconv_1)
{
  struct lconv *p;
  setlocale(LC_ALL, "C");
  p = localeconv();
  TEST_FAIL_IF(strcmp(p->decimal_point, ".") != 0);
  TEST_FAIL_IF(strcmp(p->thousands_sep, "") != 0);
  TEST_FAIL_IF(strcmp(p->grouping, "") != 0);
  TEST_FAIL_IF(strcmp(p->int_curr_symbol, "") != 0);
  TEST_FAIL_IF(strcmp(p->currency_symbol, "") != 0);
  TEST_FAIL_IF(strcmp(p->mon_decimal_point, "") != 0);
  TEST_FAIL_IF(strcmp(p->mon_grouping, "") != 0);
  TEST_FAIL_IF(strcmp(p->positive_sign, "") != 0);
  TEST_FAIL_IF(strcmp(p->negative_sign, "") != 0);
  fprintf(stderr, "ifd\n%d\n", p->int_frac_digits);
  TEST_FAIL_IF(p->int_frac_digits != CHAR_MAX);
  TEST_FAIL_IF(p->frac_digits != CHAR_MAX);
  TEST_FAIL_IF(p->p_cs_precedes != CHAR_MAX);
  TEST_FAIL_IF(p->p_sep_by_space != CHAR_MAX);
  TEST_FAIL_IF(p->n_cs_precedes != CHAR_MAX);
  TEST_FAIL_IF(p->n_sep_by_space != CHAR_MAX);
  TEST_FAIL_IF(p->p_sign_posn != CHAR_MAX);
  TEST_FAIL_IF(p->n_sign_posn != CHAR_MAX);

  TEST_FAIL_IF(p->int_p_cs_precedes != CHAR_MAX);
  TEST_FAIL_IF(p->int_n_cs_precedes != CHAR_MAX);
  TEST_FAIL_IF(p->int_p_sep_by_space != CHAR_MAX);
  TEST_FAIL_IF(p->int_n_sep_by_space != CHAR_MAX);
  TEST_FAIL_IF(p->int_p_sign_posn != CHAR_MAX);
  TEST_FAIL_IF(p->int_n_sign_posn != CHAR_MAX);
  TEST(1);
}
