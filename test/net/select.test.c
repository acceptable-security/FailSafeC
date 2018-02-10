/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file net/select.test.c
 */
#include <sys/types.h>
#include <sys/select.h>
#include <unistd.h>
#include <poll.h>
#include <errno.h>

#include "common.h"

static int rfd[3], wfd[3];
static fd_set rset, wset, eset;

static void set_fd_set()
{
  int i;

  FD_ZERO(&rset);
  FD_ZERO(&wset);
  FD_ZERO(&eset);

  for (i = 0; i < 3; i++) {
    FD_SET(rfd[i], &rset);
    FD_SET(wfd[i], &wset);
  }
}

/**
 * @testname select_1
 * @testfor select
 */
TEST_CASE(select_1)
{
  char buf[256];
  int fd[2];
  int i, ret;
  struct timeval tv;

  for (i = 0; i < 3; i++) {
    pipe(fd);
    rfd[i] = dup2(fd[0], FD_SETSIZE - 1 - i);
    wfd[i] = fd[1];
    close(fd[0]);
  }

  set_fd_set();
  ret = select(rfd[0]+1, &rset, &wset, &eset, 0);
  TEST_FAIL_IF(ret != 3);
  TEST_FAIL_IF(!FD_ISSET(wfd[0], &wset));
  TEST_FAIL_IF(!FD_ISSET(wfd[1], &wset));
  TEST_FAIL_IF(!FD_ISSET(wfd[2], &wset));

  set_fd_set();
  tv.tv_sec = 0;
  tv.tv_usec = 10000;
  ret = select(rfd[0]+1, &rset, 0, 0, &tv);
  TEST_FAIL_IF(ret != 0);

  write(wfd[0], "abcd", 4);
  set_fd_set();
  ret = select(rfd[0]+1, &rset, 0, 0, 0);
  TEST_FAIL_IF(ret != 1);
  TEST_FAIL_IF(!FD_ISSET(rfd[0], &rset));
  TEST_FAIL_IF(FD_ISSET(rfd[1], &rset));
  TEST_FAIL_IF(FD_ISSET(rfd[2], &rset));

  read(rfd[0], buf, 4);
  set_fd_set();
  ret = select(rfd[0]+1, &rset, &wset, &eset, 0);
  TEST_FAIL_IF(ret != 3);
  TEST_FAIL_IF(!FD_ISSET(wfd[0], &wset));
  TEST_FAIL_IF(!FD_ISSET(wfd[1], &wset));
  TEST_FAIL_IF(!FD_ISSET(wfd[2], &wset));
  TEST(1);
}

static struct pollfd pollfds[6];

static void set_pollfd(void)
{
  int i;
  for (i = 0; i < 3; i++) {
    pollfds[i].fd = rfd[i];
    pollfds[i].events = POLLIN;
    pollfds[i+3].fd = wfd[i];
    pollfds[i+3].events = POLLOUT;
  }
}

/**
 * @testname poll_1
 * @testfor poll
 */
TEST_CASE(poll_1)
{
  char buf[256];
  int fd[2];
  int i, ret;
  struct timeval tv;

  for (i = 0; i < 3; i++) {
    pipe(fd);
    rfd[i] = fd[0];
    wfd[i] = fd[1];
  }
  set_pollfd();

  ret = poll(pollfds, 6, -1);
  TEST_FAIL_IF(ret != 3);
  TEST_FAIL_IF(pollfds[0].revents != 0);
  TEST_FAIL_IF(pollfds[1].revents != 0);
  TEST_FAIL_IF(pollfds[2].revents != 0);
  TEST_FAIL_IF(pollfds[3].revents != POLLOUT);
  TEST_FAIL_IF(pollfds[4].revents != POLLOUT);
  TEST_FAIL_IF(pollfds[5].revents != POLLOUT);

  write(wfd[0], "abcd", 4);
  ret = poll(pollfds, 3, -1);
  TEST_FAIL_IF(ret != 1);
  TEST_FAIL_IF(pollfds[0].revents != POLLIN);
  TEST_FAIL_IF(pollfds[1].revents != 0);
  TEST_FAIL_IF(pollfds[2].revents != 0);

  read(rfd[0], buf, 4);
  ret = poll(pollfds, 3, 0);
  TEST_FAIL_IF(ret != 0);

  TEST(1);
}

/**
 * @testname pselect_1
 * @testfor pselect
 */
TEST_CASE(pselect_1)
{
  char buf[256];
  int fd[2];
  int i, ret;
  struct timespec tv;

  for (i = 0; i < 3; i++) {
    pipe(fd);
    rfd[i] = dup2(fd[0], FD_SETSIZE - 1 - i);
    wfd[i] = fd[1];
    close(fd[0]);
  }

  set_fd_set();
  ret = pselect(rfd[0]+1, &rset, &wset, &eset, 0, 0);
  TEST_FAIL_IF(ret != 3);
  TEST_FAIL_IF(!FD_ISSET(wfd[0], &wset));
  TEST_FAIL_IF(!FD_ISSET(wfd[1], &wset));
  TEST_FAIL_IF(!FD_ISSET(wfd[2], &wset));

  set_fd_set();
  tv.tv_sec = 0;
  tv.tv_nsec = 10000 * 1000;
  ret = pselect(rfd[0]+1, &rset, 0, 0, &tv, 0);
  TEST_FAIL_IF(ret != 0);

  write(wfd[0], "abcd", 4);
  set_fd_set();
  ret = pselect(rfd[0]+1, &rset, 0, 0, 0, 0);
  TEST_FAIL_IF(ret != 1);
  TEST_FAIL_IF(!FD_ISSET(rfd[0], &rset));
  TEST_FAIL_IF(FD_ISSET(rfd[1], &rset));
  TEST_FAIL_IF(FD_ISSET(rfd[2], &rset));

  read(rfd[0], buf, 4);
  set_fd_set();
  ret = pselect(rfd[0]+1, &rset, &wset, &eset, 0, 0);
  TEST_FAIL_IF(ret != 3);
  TEST_FAIL_IF(!FD_ISSET(wfd[0], &wset));
  TEST_FAIL_IF(!FD_ISSET(wfd[1], &wset));
  TEST_FAIL_IF(!FD_ISSET(wfd[2], &wset));
  TEST(1);
}


void alarm_handler(int s) {
  signal(SIGALRM, alarm_handler);
}

/**
 * @testname pselect_2
 * @testfor pselect
 */
TEST_CASE(pselect_2)
{
  sigset_t mask;
  struct timespec ts;
  int r;

  ts.tv_sec = 2;
  ts.tv_nsec = 0;

  signal(SIGALRM, alarm_handler);

  sigemptyset(&mask);
  alarm(1);
  r = pselect(0, 0, 0, 0, &ts, &mask);
  TEST_FAIL_IF(r != -1);
  TEST_FAIL_IF(EINTR != errno);

  sigaddset(&mask, SIGALRM);
  alarm(1);
  r = pselect(0, 0, 0, 0, &ts, &mask);
  TEST_FAIL_IF(r == -1);

  TEST(1);
}
