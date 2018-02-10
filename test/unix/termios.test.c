/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file unix/termios.test.c
 */
#include "common.h"
#include <termios.h>
#include <string.h>
#include <unistd.h>

#include <stdio.h>
#include <errno.h>

static int pty_fd(void)
{
  return atoi(getenv("TEST_PTY_FD"));
}

/**
 * @testname tcgetattr_1
 * @testfor tcgetattr
 */
TEST_CASE(tcgetattr_1)
{
  struct termios t;

  TEST(tcgetattr(pty_fd(), &t) == 0);
}

/**
 * @testname tcsetattr_1
 * @testfor tcsetattr
 */

static void dump(void *m, int s, char *l) {
    unsigned char *c = m;
    int i;
    fprintf(stderr, "%s:", l);
    for(i = 0; i < s; i++) {
	fprintf(stderr, " %02x", c[i]);
    };
    fprintf(stderr, "\n");
}

TEST_CASE(tcsetattr_1)
{
  struct termios o, t, d;
  int i;
  int fd = pty_fd();

  fprintf(stderr, "fd: %d\n", fd);

  TEST_FAIL_IF(tcgetattr(fd, &t));
  memcpy(&d, &t, sizeof(struct termios));
  memcpy(&o, &t, sizeof(struct termios));

  d.c_iflag ^= INLCR;
  d.c_oflag ^= ONLCR;
  d.c_cflag ^= CSTOPB;
  d.c_lflag ^= ECHO;

  alarm(5);

  do{
    dump(&d, sizeof d, "setting");
    TEST_FAIL_IF(tcsetattr(fd, TCSANOW, &d));
    TEST_FAIL_IF(tcgetattr(fd, &t));
    dump(&t, sizeof t, "got    ");
  }while(memcmp(&d, &t, sizeof(struct termios)));

  do{
    TEST_FAIL_IF(tcsetattr(fd, TCSANOW, &o));
    TEST_FAIL_IF(tcgetattr(fd, &t));
  }while(memcmp(&o, &t, sizeof(struct termios)));

  TEST(1);
}

/**
 * @testname cfgetsetispeed
 * @testfor cfgetispeed
 * @testfor cfsetispeed
 */
TEST_CASE(cfgetsetispeed)
{
  struct termios d;
  int fd = pty_fd();

  TEST_FAIL_IF(tcgetattr(fd, &d));
  TEST_FAIL_IF(cfsetispeed(&d, B38400) != 0);
  TEST_FAIL_IF(cfgetispeed(&d) != B38400);
  TEST_FAIL_IF(cfsetispeed(&d, B19200) != 0);
  TEST_FAIL_IF(cfgetispeed(&d) != B19200);
  TEST(1);
}

/**
 * @testname cfgetsetospeed
 * @testfor cfgetospeed
 * @testfor cfsetospeed
 */
TEST_CASE(cfgetsetospeed)
{
  struct termios d;
  int fd = pty_fd();

  TEST_FAIL_IF(tcgetattr(fd, &d));
  TEST_FAIL_IF(cfsetospeed(&d, B38400) != 0);
  TEST_FAIL_IF(cfgetospeed(&d) != B38400);
  TEST_FAIL_IF(cfsetospeed(&d, B19200) != 0);
  TEST_FAIL_IF(cfgetospeed(&d) != B19200);
  TEST(1);
}
