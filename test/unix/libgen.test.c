/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file unix/libgen.test.c
 */
#include <libgen.h>
#include <string.h>

#include "common.h"

static int basename_dirname_check(char *src, char *base, char *dir)
{
  char *p = strdup(src);
  char *q = strdup(src);
  int b = strcmp(basename(p), base) == 0;
  int d = strcmp(dirname(q), dir) == 0;
  free(p);
  free(q);
  return b && d;
}

/**
 * @testname basename_dirname
 * @testfor basename
 * @testfor dirname
 */
TEST_CASE(basename_dirname)
{
  TEST_FAIL_IF(!basename_dirname_check("/usr/lib", "lib", "/usr"));
  TEST_FAIL_IF(!basename_dirname_check("/usr/", "usr", "/"));
  TEST_FAIL_IF(!basename_dirname_check("/", "/", "/"));
  TEST_FAIL_IF(!basename_dirname_check("///", "/", "/"));
  TEST_FAIL_IF(!basename_dirname_check("//usr//lib//", "lib", "//usr"));
  TEST_FAIL_IF(!basename_dirname_check(".", ".", "."));
  TEST_FAIL_IF(!basename_dirname_check("..", "..", "."));
  TEST(1);
}
