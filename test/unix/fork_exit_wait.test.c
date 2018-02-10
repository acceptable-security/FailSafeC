/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file unix/fork_exit_wait.test.c
 */
/*
 * int fork(void)
 * void _exit(void)
 * int wait(int *status)
 */
#include <sys/types.h>
#include <sys/wait.h>
#include <unistd.h>
#include <stdlib.h>

#include "common.h"

static pid_t pid;
static int status;

static void fork_exit_35(void)
{
  pid = fork();
  TEST_FAIL_IF(pid < 0);
  if (pid == 0) {
    _exit(35);
  }
}

/**
 * @testname fork_exit_wait_1
 * @testfor fork
 * @testfor _exit
 * @testfor wait
 */
TEST_CASE(fork_exit_wait_1)
{
  fork_exit_35();
  TEST_FAIL_IF(wait(&status) != pid);
  TEST((status & 0xFF00) >> 8 == 35);
}

/**
 * @testname fork_exit_wait_2
 * @testfor fork
 * @testfor _exit
 * @testfor waitpid
 */
TEST_CASE(fork_exit_wait_2)
{
  fork_exit_35();
  TEST_FAIL_IF(waitpid(pid, &status, 0) != pid);
  TEST((status & 0xFF00) >> 8 == 35);
}

/**
 * @testname fork_exit_wait_3
 * @testfor fork
 * @testfor _exit
 * @testfor wait
 */
TEST_CASE(fork_exit_wait_3)
{
  pid = wait(&status);
  TEST(pid == -1);
}

/**
 * @testname fork_exit_wait_4s
 * @testfor fork
 * @testfor _exit
 * @testfor wait
 */
TEST_CASE_S(fork_exit_wait_4s, FSC_ABRT)
{
  int st[3];
  fork_exit_35();
  TEST_EFAULT_IF(wait(&st[3]) < 0);
  TEST_FAILED;
}

/**
 * @testname fork_exit_wait_5s
 * @testfor fork
 * @testfor _exit
 * @testfor wait
 */
TEST_CASE_S(fork_exit_wait_5s, FSC_ABRT)
{
  int st[3];
  fork_exit_35();
  TEST_EFAULT_IF(wait(&st[-1]) < 0);
  TEST_FAILED;
}

/**
 * @testname fork_exit_wait_6s
 * @testfor fork
 * @testfor _exit
 * @testfor waitpid
 */
TEST_CASE_S(fork_exit_wait_6s, FSC_ABRT)
{
  int st[3];
  fork_exit_35();
  TEST_EFAULT_IF(waitpid(-1, &st[3], WNOHANG | WUNTRACED) < 0);
  TEST_FAILED;
}

/**
 * @testname fork_exit_wait_7s
 * @testfor fork
 * @testfor _exit
 * @testfor waitpid
 */
TEST_CASE_S(fork_exit_wait_7s, FSC_ABRT)
{
  int st[3];
  fork_exit_35();
  TEST_EFAULT_IF(waitpid(-1, &st[-1], WNOHANG | WUNTRACED) < 0);
  TEST_FAILED;
}

/**
 * @testname fork_exit_wait_8
 * @testfor _exit
 */
TEST_CASE(fork_exit_wait_8)
{
  _exit(0);
  TEST_FAILED;
}
