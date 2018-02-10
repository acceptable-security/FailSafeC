/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <unistd.h>

static int pty_fd(void)
{
  return atoi(getenv("TEST_PTY_FD"));
}

/**
 * @testname isatty_1
 * @testfor isatty
 */
TEST_CASE(isatty_1)
{
  TEST(isatty(pty_fd()) == 1);
}

/**
 * @testname isatty_2
 * @testfor isatty
 */
TEST_CASE(isatty_2)
{
  TEST(isatty(1) == 0);
}
