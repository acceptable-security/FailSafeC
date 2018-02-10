/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file test/unix/exec.test.c
 */
#include <unistd.h>
#include "common.h"

/**
 * @testname execve_1
 * @testfor execve
 */
TEST_CASE(execve_1)
{
  char *argv[] = { NULL };
  char *envp[] = { NULL };

  TEST(execve("/bin/no such executable", argv, envp) == -1);
}

/**
 * @testname execve_2
 * @testfor execve
 */
TEST_CASE(execve_2)
{
  char *argv[] = { NULL };
  char *envp[] = { NULL };

  execve("/bin/true", argv, envp);
  TEST(0);
}

/**
 * @testname execve_3
 * @testfor execve
 */
TEST_CASE(execve_3)
{
  char *argv[] = { "/bin/sh", "-c", "exit 0", NULL };
  char *envp[] = { NULL };

  execve("/bin/sh", argv, envp);
  TEST(0);
}

/**
 * @testname execve_4
 * @testfor execve
 */
TEST_CASE(execve_4)
{
  char *argv[] = { "/bin/sh", "-c", "! test -z \"$X\"", NULL };
  char *envp[] = { "X=xyzzy", NULL };

  execve("/bin/sh", argv, envp);
  TEST(0);
}

/**
 * @testname execve_5
 * @testfor execve
 */
TEST_CASE(execve_5)
{
  char *argv[] = { NULL };
  char *envp[] = { NULL };

  TEST(execve("true", argv, envp) == -1);
}

/**
 * @testname execv_1
 * @testfor execv
 */
TEST_CASE(execv_1)
{
  char *argv[] = { NULL };

  TEST(execv("/bin/no such executable", argv) == -1);
}

/**
 * @testname execv_2
 * @testfor execv
 */
TEST_CASE(execv_2)
{
  char *argv[] = { NULL };

  execv("/bin/true", argv);
  TEST(0);
}

/**
 * @testname execv_3
 * @testfor execv
 */
TEST_CASE(execv_3)
{
  char *argv[] = { "/bin/sh", "-c", "exit 0", NULL };

  execv("/bin/sh", argv);
  TEST(0);
}

/**
 * @testname execv_4
 * @testfor execv
 */
TEST_CASE(execv_4)
{
  char *argv[] = { NULL };

  TEST(execv("true", argv) == -1);
}

/**
 * @testname execvp_1
 * @testfor execvp
 */
TEST_CASE(execvp_1)
{
  char *argv[] = { NULL };

  TEST(execvp("no such executable", argv) == -1);
}

/**
 * @testname execvp_2
 * @testfor execvp
 */
TEST_CASE(execvp_2)
{
  char *argv[] = { NULL };

  execvp("true", argv);
  TEST(0);
}

/**
 * @testname execvp_3
 * @testfor execvp
 */
TEST_CASE(execvp_3)
{
  char *argv[] = { "/bin/sh", "-c", "exit 0", NULL };

  execvp("sh", argv);
  TEST(0);
}

/**
 * @testname execl_1
 * @testfor execl
 */
TEST_CASE(execl_1)
{
  TEST(execl("/bin/no such executable", (char *)NULL) == -1);
}

/**
 * @testname execl_2
 * @testfor execl
 */
TEST_CASE(execl_2)
{
  execl("/bin/true", (char *)NULL);
  TEST(0);
}

/**
 * @testname execl_3
 * @testfor execl
 */
TEST_CASE(execl_3)
{
  execl("/bin/sh", "/bin/sh", "-c", "exit 0", (char *)NULL);
  TEST(0);
}

/**
 * @testname execl_4
 * @testfor execl
 */
TEST_CASE(execl_4)
{
  TEST(execl("true", (char *)NULL) == -1);
}

/**
 * @testname execlp_1
 * @testfor execlp
 */
TEST_CASE(execlp_1)
{
  TEST(execlp("no such executable", (char *)NULL) == -1);
}

/**
 * @testname execlp_2
 * @testfor execlp
 */
TEST_CASE(execlp_2)
{
  execlp("true", (char *)NULL);
  TEST(0);
}

/**
 * @testname execlp_3
 * @testfor execlp
 */
TEST_CASE(execlp_3)
{
  execlp("sh", "/bin/sh", "-c", "exit 0", (char *)NULL);
  TEST(0);
}

/**
 * @testname execle_1
 * @testfor execle
 */
TEST_CASE(execle_1)
{
  char *envp[] = { "X=xyzzy", NULL };
  TEST(execle("no such executable", (char *)NULL, envp) == -1);
}

/**
 * @testname execle_2
 * @testfor execle
 */
TEST_CASE(execle_2)
{
  char *envp[] = { "X=xyzzy", NULL };
  execle("/bin/true", (char *)NULL, envp);
  TEST(0);
}

/**
 * @testname execle_3
 * @testfor execle
 */
TEST_CASE(execle_3)
{
  char *envp[] = { "X=xyzzy", NULL };
  execle("/bin/sh", "/bin/sh", "-c", "exit 0", (char *)NULL, envp);
  TEST(0);
}

/**
 * @testname execle_4
 * @testfor execle
 */
TEST_CASE(execle_4)
{
  char *envp[] = { "X=xyzzy", NULL };

  execle("/bin/sh", "sh", "-c", "! test -z \"$X\"", (char *)NULL, envp);
  TEST(0);
}
