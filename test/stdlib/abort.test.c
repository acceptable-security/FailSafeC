/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file stdlib/abort.test.c
 */

#include <stdlib.h>
#include "common.h"

/**
 * @testname abort_1
 * @testfor abort
 */
TEST_CASE_S(abort_1, SIGABRT)
{
  abort();
  TEST_FAILED;
}
