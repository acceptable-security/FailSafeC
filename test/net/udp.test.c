/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file net/udp.test.c
 */
#include "common.h"
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include <signal.h>
#include <unistd.h>

#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

static const char data[16] = "-UDP ECHO TEST-";

static int get_udp_port(){
  int port = atoi(getenv("TEST_SERVER_UDP_PORT"));
  return port;
}

void fail_handler(int n){
  TEST_FAILED;
}

/**
 * @testname sendto_1
 * @testfor sendto
 */
TEST_CASE(sendto_1)
{
  struct sockaddr_in addr;
  int s = socket(AF_INET, SOCK_DGRAM, 0);
  TEST_FAIL_IF(s == -1);

  signal(SIGALRM, fail_handler);
  alarm(5);

  addr.sin_family = AF_INET;
  addr.sin_port = htons(get_udp_port());
  addr.sin_addr.s_addr = htonl(0x7f000001);
  TEST_FAIL_IF(16 != sendto(s, data, 16, 0, (struct sockaddr *)&addr, sizeof(addr)));

  TEST(1);
}

/**
 * @testname recvfrom_1
 * @testfor recvfrom
 */
TEST_CASE(recvfrom_1)
{
  struct sockaddr_in addr;
  char buf[32];
  int addr_len;
  int s = socket(AF_INET, SOCK_DGRAM, 0);
  TEST_FAIL_IF(s == -1);

  signal(SIGALRM, fail_handler);
  alarm(5);

  addr.sin_family = AF_INET;
  addr.sin_port = htons(get_udp_port());
  addr.sin_addr.s_addr = htonl(0x7f000001);
  TEST_FAIL_IF(16 != sendto(s, data, 16, 0, (struct sockaddr *)&addr, sizeof(addr)));

  addr_len = sizeof(addr);
  TEST_FAIL_IF(16 != recvfrom(s, buf, 32, 0, (struct sockaddr *)&addr, &addr_len));
  TEST_FAIL_IF(memcmp(data, buf, 16) != 0);

  TEST(1);
}

