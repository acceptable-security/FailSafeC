/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"
#include <sys/socket.h>
#include <memory.h>
#include <netinet/in.h>
#include <arpa/inet.h>

static int get_tcp_port(){
  int port = atoi(getenv("TEST_SERVER_TCP_PORT"));
  return port;
}

/**
 * @testname getpeername_1
 * @testfor getpeername
 */
TEST_CASE(getpeername_1)
{
  struct sockaddr sa;
  socklen_t sz;

  sz = sizeof(sa);
  TEST(-1 == getpeername(1, &sa, &sz));
}

/**
 * @testname getpeername_2
 * @testfor getpeername
 */
TEST_CASE(getpeername_2)
{
  int sock;
  struct sockaddr sa;
  socklen_t sz;

  sock = socket(AF_INET, SOCK_STREAM, 0);
  TEST_FAIL_IF(sock == -1);

  sz = sizeof(sa);
  TEST(-1 == getpeername(sock, &sa, &sz));
}

/**
 * @testname getpeername_3
 * @testfor getpeername
 */
TEST_CASE(getpeername_3)
{
  int sock;
  struct sockaddr_in sa;
  socklen_t sz;

  sock = socket(AF_INET, SOCK_STREAM, 0);
  TEST_FAIL_IF(sock == -1);

  sa.sin_family = AF_INET;
  sa.sin_port = htons(get_tcp_port());
  sa.sin_addr.s_addr = htonl(0x7f000001);
  TEST_FAIL_IF(connect(sock, (struct sockaddr *)&sa, sizeof(sa)) == -1);

  sz = sizeof(sa);
  memset(&sa, 0, sizeof(sa));
  TEST_FAIL_IF(-1 == getpeername(sock, (struct sockaddr *)&sa, &sz));
  TEST_FAIL_IF(sz != sizeof(sa));
  TEST_FAIL_IF(sa.sin_port != htons(get_tcp_port()));
  TEST_FAIL_IF(sa.sin_addr.s_addr != htonl(0x7f000001));
  TEST(1);
}
