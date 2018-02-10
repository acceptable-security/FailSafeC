/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file unix/fnmatch.test.c
 */
#include <fnmatch.h>
#include <stdio.h>
#include "common.h"

/**
 * @testname fnmatch_1
 * @testfor fnmatch
 */
TEST_CASE(fnmatch_1)
{
  int flags[] = {
    0, FNM_PATHNAME, FNM_NOESCAPE, FNM_PERIOD,
    FNM_PATHNAME|FNM_NOESCAPE, FNM_NOESCAPE|FNM_PERIOD, FNM_PATHNAME|FNM_PERIOD,
    FNM_PERIOD|FNM_NOESCAPE|FNM_PERIOD
  };
  int i;
  for (i = 0; i < 8; i++) {
    int f = flags[i];
    int path = f & FNM_PATHNAME;
    int noes = f & FNM_NOESCAPE;
    int peri = f & FNM_PERIOD;
    int match = 0;
    int nomatch = FNM_NOMATCH;
    TEST_FAIL_IF(fnmatch("*", "foo/.emacs", f) != (path ? nomatch : match));
    TEST_FAIL_IF(fnmatch("*/*", "foo/.emacs", f) != (path&&peri ? nomatch : match));
    TEST_FAIL_IF(fnmatch("*", ".emacs", f) != (peri ? nomatch : match));
    TEST_FAIL_IF(fnmatch("\\\\", "\\", f) != (noes ? nomatch : match));
  }
  TEST(1);
}
