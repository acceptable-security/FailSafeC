/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file stdlib/signal.test.c
 */

#include <sys/types.h>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include "common.h"

/**
 * @testname raise_1
 * @testfor raise
 */
TEST_CASE_S(raise_1, SIGABRT)
{
  raise(SIGABRT);
}

/**
 * @testname kill_1
 * @testfor kill
 */
TEST_CASE_S(kill_1, SIGABRT)
{
  kill(getpid(), SIGABRT);
}

void handler(int i){
  TEST_OK(1);
}

void badhandler(int i){
  TEST_FAILED;
}

void handler2(int s, siginfo_t *info, void *p){
  TEST_FAIL_IF(s != SIGABRT);
  TEST_FAIL_IF(info->si_signo != SIGABRT);
  TEST_OK(1);
}

/**
 * @testname signal_1
 * @testfor signal
 */
TEST_CASE(signal_1)
{
  signal(SIGABRT, handler);
  raise(SIGABRT);
  TEST_FAILED;
}

/**
 * @testname sigaction_1
 * @testfor sigaction
 */
TEST_CASE(sigaction_1)
{
  struct sigaction new, old;

  new.sa_handler = handler;
  new.sa_flags = 0;
  sigemptyset(&new.sa_mask);

  sigaction(SIGABRT, &new, NULL);
  sigaction(SIGABRT, &new, &old);

  TEST_FAIL_IF(new.sa_handler != old.sa_handler);

  new.sa_handler = badhandler;
  sigaction(SIGABRT, &new, &old);

  TEST(handler == old.sa_handler);
}

/**
 * @testname sigaction_2
 * @testfor sigaction
 */
TEST_CASE(sigaction_2)
{
  struct sigaction new;

  new.sa_handler = handler;
  new.sa_flags = 0;
  sigemptyset(&new.sa_mask);

  sigaction(SIGABRT, &new, NULL);

  raise(SIGABRT);
  TEST_FAILED;
}

/**
 * @testname sigaction_3
 * @testfor sigaction
 */
TEST_CASE(sigaction_3)
{
  struct sigaction new;

  new.sa_handler = badhandler;
  new.sa_sigaction = handler2;
  new.sa_flags = SA_SIGINFO;
  sigemptyset(&new.sa_mask);

  sigaction(SIGABRT, &new, NULL);

  raise(SIGABRT);
  TEST_FAILED;
}

/**
 * @testname sigprocmask_1
 * @testfor sigprocmask
 */
TEST_CASE(sigprocmask_1)
{
  sigset_t set, oset;

  TEST_FAIL_IF(0 != sigemptyset(&set));
  TEST_FAIL_IF(0 != sigprocmask(SIG_SETMASK, &set, &oset));

  TEST_FAIL_IF(0 != sigaddset(&set, SIGABRT));
  TEST_FAIL_IF(0 != sigaddset(&set, SIGALRM));
  TEST_FAIL_IF(0 != sigprocmask(SIG_BLOCK, &set, &oset));
  TEST_FAIL_IF(1 == sigismember(&oset, SIGABRT));
  TEST_FAIL_IF(1 == sigismember(&oset, SIGALRM));

  TEST_FAIL_IF(0 != sigprocmask(SIG_UNBLOCK, &set, &oset));
  TEST_FAIL_IF(0 == sigismember(&oset, SIGABRT));
  TEST_FAIL_IF(0 == sigismember(&oset, SIGALRM));

  TEST_FAIL_IF(0 != sigprocmask(SIG_BLOCK, &set, &oset));
  TEST_FAIL_IF(1 == sigismember(&oset, SIGABRT));
  TEST_FAIL_IF(1 == sigismember(&oset, SIGALRM));

  TEST_FAIL_IF(0 != sigdelset(&set, SIGALRM));
  TEST_FAIL_IF(0 != sigprocmask(SIG_SETMASK, &set, &oset));
  TEST_FAIL_IF(0 == sigismember(&oset, SIGABRT));
  TEST_FAIL_IF(0 == sigismember(&oset, SIGALRM));

  TEST_FAIL_IF(0 != sigemptyset(&set));
  TEST_FAIL_IF(0 != sigprocmask(SIG_SETMASK, &set, &oset));
  TEST_FAIL_IF(0 == sigismember(&oset, SIGABRT));
  TEST_FAIL_IF(1 == sigismember(&oset, SIGALRM));

  TEST(1);
}

/**
 * @testname sigprocmask_2
 * @testfor sigprocmask
 */
TEST_CASE(sigprocmask_2)
{
  sigset_t set[4];
  int i;

  TEST_FAIL_IF(0 != sigprocmask(SIG_SETMASK, NULL, &set[0]));
  TEST_FAIL_IF(0 != sigprocmask(SIG_BLOCK,   NULL, &set[1]));
  TEST_FAIL_IF(0 != sigprocmask(SIG_UNBLOCK, NULL, &set[2]));
  TEST_FAIL_IF(0 != sigprocmask(12345,       NULL, &set[3]));

  for (i = 0; i < _NSIG; i++) {
    TEST_FAIL_IF(sigismember(&set[0], i) != sigismember(&set[1], i));
    TEST_FAIL_IF(sigismember(&set[1], i) != sigismember(&set[2], i));
    TEST_FAIL_IF(sigismember(&set[2], i) != sigismember(&set[3], i));
  }

  TEST(1);
}

/**
 * @testname sigprocmask_3
 * @testfor sigprocmask
 */
TEST_CASE(sigprocmask_3)
{
  sigset_t set, oset;

  TEST_FAIL_IF(0 != sigemptyset(&set));
  TEST_FAIL_IF(0 != sigprocmask(SIG_SETMASK, &set, NULL));
  TEST_FAIL_IF(0 != sigprocmask(SIG_SETMASK, NULL, &oset));
  TEST_FAIL_IF(1 == sigismember(&oset, SIGABRT));
  TEST_FAIL_IF(1 == sigismember(&oset, SIGALRM));

  TEST_FAIL_IF(0 != sigaddset(&set, SIGABRT));
  TEST_FAIL_IF(0 != sigaddset(&set, SIGALRM));
  TEST_FAIL_IF(0 != sigprocmask(SIG_BLOCK, &set, NULL));
  TEST_FAIL_IF(0 != sigprocmask(SIG_SETMASK, NULL, &oset));
  TEST_FAIL_IF(0 == sigismember(&oset, SIGABRT));
  TEST_FAIL_IF(0 == sigismember(&oset, SIGALRM));

  TEST_FAIL_IF(0 != sigprocmask(SIG_UNBLOCK, &set, NULL));
  TEST_FAIL_IF(0 != sigprocmask(SIG_SETMASK, NULL, &oset));
  TEST_FAIL_IF(1 == sigismember(&oset, SIGABRT));
  TEST_FAIL_IF(1 == sigismember(&oset, SIGALRM));

  TEST_FAIL_IF(0 != sigprocmask(SIG_BLOCK, &set, NULL));
  TEST_FAIL_IF(0 != sigprocmask(SIG_SETMASK, NULL, &oset));
  TEST_FAIL_IF(0 == sigismember(&oset, SIGABRT));
  TEST_FAIL_IF(0 == sigismember(&oset, SIGALRM));

  TEST_FAIL_IF(0 != sigdelset(&set, SIGALRM));
  TEST_FAIL_IF(0 != sigprocmask(SIG_SETMASK, &set, NULL));
  TEST_FAIL_IF(0 != sigprocmask(SIG_SETMASK, NULL, &oset));
  TEST_FAIL_IF(0 == sigismember(&oset, SIGABRT));
  TEST_FAIL_IF(1 == sigismember(&oset, SIGALRM));

  TEST_FAIL_IF(0 != sigemptyset(&set));
  TEST_FAIL_IF(0 != sigprocmask(SIG_SETMASK, &set, NULL));
  TEST_FAIL_IF(0 != sigprocmask(SIG_SETMASK, NULL, &oset));
  TEST_FAIL_IF(1 == sigismember(&oset, SIGABRT));
  TEST_FAIL_IF(1 == sigismember(&oset, SIGALRM));

  TEST(1);
}

/**
 * @testname sighold_1
 * @testfor sighold
 */
TEST_CASE(sighold_1)
{
  sigset_t set, oset;

  TEST_FAIL_IF(sigemptyset(&set) != 0);
  TEST_FAIL_IF(sigprocmask(SIG_SETMASK, &set, NULL) != 0);

  TEST_FAIL_IF(sighold(SIGALRM) != 0);
  TEST_FAIL_IF(sigprocmask(SIG_SETMASK, NULL, &set) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGALRM) != 1);
  TEST_FAIL_IF(sigismember(&set, SIGABRT) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGUSR1) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGUSR2) != 0);

  TEST_FAIL_IF(sighold(SIGABRT) != 0);
  TEST_FAIL_IF(sigprocmask(SIG_SETMASK, NULL, &set) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGALRM) != 1);
  TEST_FAIL_IF(sigismember(&set, SIGABRT) != 1);
  TEST_FAIL_IF(sigismember(&set, SIGUSR1) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGUSR2) != 0);

  TEST_FAIL_IF(sighold(SIGUSR1) != 0);
  TEST_FAIL_IF(sigprocmask(SIG_SETMASK, NULL, &set) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGALRM) != 1);
  TEST_FAIL_IF(sigismember(&set, SIGABRT) != 1);
  TEST_FAIL_IF(sigismember(&set, SIGUSR1) != 1);
  TEST_FAIL_IF(sigismember(&set, SIGUSR2) != 0);

  TEST_FAIL_IF(sighold(SIGUSR2) != 0);
  TEST_FAIL_IF(sigprocmask(SIG_SETMASK, NULL, &set) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGALRM) != 1);
  TEST_FAIL_IF(sigismember(&set, SIGABRT) != 1);
  TEST_FAIL_IF(sigismember(&set, SIGUSR1) != 1);
  TEST_FAIL_IF(sigismember(&set, SIGUSR2) != 1);

  TEST(1);
}

/**
 * @testname sigrelse_1
 * @testfor sigrelse
 */
TEST_CASE(sigrelse_1)
{
  sigset_t set, oset;

  TEST_FAIL_IF(sigfillset(&set) != 0);
  TEST_FAIL_IF(sigprocmask(SIG_SETMASK, &set, NULL) != 0);

  TEST_FAIL_IF(sigrelse(SIGALRM) != 0);
  TEST_FAIL_IF(sigprocmask(SIG_SETMASK, NULL, &set) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGALRM) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGABRT) != 1);
  TEST_FAIL_IF(sigismember(&set, SIGUSR1) != 1);
  TEST_FAIL_IF(sigismember(&set, SIGUSR2) != 1);

  TEST_FAIL_IF(sigrelse(SIGABRT) != 0);
  TEST_FAIL_IF(sigprocmask(SIG_SETMASK, NULL, &set) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGALRM) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGABRT) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGUSR1) != 1);
  TEST_FAIL_IF(sigismember(&set, SIGUSR2) != 1);

  TEST_FAIL_IF(sigrelse(SIGUSR1) != 0);
  TEST_FAIL_IF(sigprocmask(SIG_SETMASK, NULL, &set) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGALRM) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGABRT) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGUSR1) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGUSR2) != 1);

  TEST_FAIL_IF(sigrelse(SIGUSR2) != 0);
  TEST_FAIL_IF(sigprocmask(SIG_SETMASK, NULL, &set) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGALRM) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGABRT) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGUSR1) != 0);
  TEST_FAIL_IF(sigismember(&set, SIGUSR2) != 0);

  TEST(1);
}

/**
 * @testname sigignore_1
 * @testfor sigignore
 */
TEST_CASE(sigignore_1)
{
  TEST_FAIL_IF(SIG_ERR == signal(SIGABRT, handler));

  TEST_FAIL_IF(0 != sigignore(SIGABRT));
  TEST_FAIL_IF(SIG_IGN != signal(SIGABRT, SIG_DFL));

  TEST_FAIL_IF(0 != sigignore(SIGABRT));
  TEST_FAIL_IF(SIG_IGN != signal(SIGABRT, SIG_DFL));

  TEST(1);
}

/**
 * @testname sigset_1
 * @testfor sigset
 */
TEST_CASE(sigset_1)
{
  sigset_t set;

  /* SIG_HOLD -> SIG_HOLD */
  TEST_FAIL_IF(0 != sighold(SIGABRT));
  TEST_FAIL_IF(SIG_HOLD != sigset(SIGABRT, SIG_HOLD));
  TEST_FAIL_IF(0 != sigprocmask(0, NULL, &set));
  TEST_FAIL_IF(1 != sigismember(&set, SIGABRT));

  /* -> handler */
  TEST_FAIL_IF(SIG_HOLD != sigset(SIGABRT, handler));
  TEST_FAIL_IF(0 != sigprocmask(0, NULL, &set));
  TEST_FAIL_IF(0 != sigismember(&set, SIGABRT));

  /* -> badhandler */
  TEST_FAIL_IF(handler != sigset(SIGABRT, badhandler));
  TEST_FAIL_IF(0 != sigprocmask(0, NULL, &set));
  TEST_FAIL_IF(0 != sigismember(&set, SIGABRT));

  /* -> SIG_HOLD */
  TEST_FAIL_IF(badhandler != sigset(SIGABRT, SIG_HOLD));
  TEST_FAIL_IF(0 != sigprocmask(0, NULL, &set));
  TEST_FAIL_IF(1 != sigismember(&set, SIGABRT));

  /* -> relse -> SIG_HOLD */
  TEST_FAIL_IF(0 != sigrelse(SIGABRT));
  TEST_FAIL_IF(badhandler != sigset(SIGABRT, SIG_HOLD));
  TEST_FAIL_IF(0 != sigprocmask(0, NULL, &set));
  TEST_FAIL_IF(1 != sigismember(&set, SIGABRT));

  TEST(1);
}
