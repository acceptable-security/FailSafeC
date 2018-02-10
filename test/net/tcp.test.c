/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file net/tcp.test.c
 */
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <sys/wait.h>
#include <unistd.h>
#include <string.h>

#include <stdio.h>

#include "common.h"

static char test_message[] = "hello, world!";

static void fork_connect(int port)
{
  int pid = fork();
  TEST_FAIL_IF(pid < 0);
  if (pid == 0) {
    int sock = socket(AF_INET, SOCK_STREAM, 0);
    struct sockaddr_in addr;
    char buf[1024];
    addr.sin_family = AF_INET;
    addr.sin_port = htons(port);
    addr.sin_addr.s_addr = htonl(0x7F000001);
    if (connect(sock, (struct sockaddr *)&addr, sizeof addr) != 0) {
      _exit(1);
    }
    write(sock, test_message, sizeof test_message);
    if (read(sock, buf, sizeof buf) != -1) {
      _exit(1);
    }
    close(sock);
    _exit(0);
  }
}

/**
 * @testname bind_listen_accept_getsockname_1
 * @testfor bind
 * @testfor listen
 * @testfor accept
 * @testfor getsockname
 */
TEST_CASE(bind_listen_accept_getsockname_1)
{
  int sock = socket(AF_INET, SOCK_STREAM, 0);
  int sock2;
  struct sockaddr_in addr;
  int port = 8000;
  int addr_len;
  char buf[1024];
  int len;

  alarm(5);
  
  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(0x7F000001);

  while (port < 20000) {
    addr.sin_port = htons(port);
    if (bind(sock, (struct sockaddr *)&addr, sizeof addr) == 0) {
      break;
    }
    port++;
  }
  TEST_FAIL_IF(port == 20000);
  addr_len = sizeof addr;
  TEST_FAIL_IF(getsockname(sock, (struct sockaddr *)&addr, &addr_len) != 0);
  TEST_FAIL_IF(addr.sin_family != AF_INET);
  TEST_FAIL_IF(addr.sin_port != htons(port));
  TEST_FAIL_IF(addr.sin_addr.s_addr != htonl(0x7F000001));
  
  TEST_FAIL_IF(listen(sock, 1) != 0);
  fork_connect(port);
  addr_len = sizeof addr;
  sock2 = accept(sock, (struct sockaddr *)&addr, &addr_len);
  TEST_FAIL_IF(sock2 < 0);
  len = read(sock2, buf, sizeof buf);
  TEST_FAIL_IF(len <= 0);
  TEST_FAIL_IF(strcmp(buf, test_message) != 0);
  close(sock2);
  wait(NULL);
  TEST(1);
}

static int get_tcp_port(){
  int port = atoi(getenv("TEST_SERVER_TCP_PORT"));
  return port;
}

/**
 * @testname connect_read_send_1
 * @testfor connect
 * @testfor read
 * @testfor send
 */
TEST_CASE(connect_read_send_1)
{
  int sock =socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in addr;
  char buf[1024];
  int len;

  TEST_FAIL_IF(sock == -1);
  alarm(5);

  addr.sin_family = AF_INET;
  addr.sin_port = htons(get_tcp_port());
  addr.sin_addr.s_addr = htonl(0x7F000001);
  TEST_FAIL_IF(connect(sock, (struct sockaddr *)&addr, sizeof addr) == -1);
  TEST_FAIL_IF(send(sock, test_message, sizeof test_message, 0) != sizeof test_message);
  TEST_FAIL_IF(read(sock, buf, sizeof buf) != sizeof test_message);
  TEST_FAIL_IF(strcmp(test_message, buf) != 0);
  close(sock);
  TEST(1);
}

/**
 * @testname connect_recv_send_1
 * @testfor connect
 * @testfor recv
 * @testfor send
 */
TEST_CASE(connect_recv_send_1)
{
  int sock =socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in addr;
  char buf[1024];
  int len;

  TEST_FAIL_IF(sock == -1);
  alarm(5);

  addr.sin_family = AF_INET;
  addr.sin_port = htons(get_tcp_port());
  addr.sin_addr.s_addr = htonl(0x7F000001);
  TEST_FAIL_IF(connect(sock, (struct sockaddr *)&addr, sizeof addr) == -1);
  TEST_FAIL_IF(send(sock, test_message, sizeof test_message, 0) != sizeof test_message);
  TEST_FAIL_IF(recv(sock, buf, sizeof buf, 0) != sizeof test_message);
  TEST_FAIL_IF(strcmp(test_message, buf) != 0);
  close(sock);
  TEST(1);
}

/**
 * @testname shutdown_1
 * @testfor shutdown
 */
TEST_CASE_S(shutdown_1, SIGPIPE)
{
  int s = socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in addr;

  alarm(5);

  addr.sin_family = AF_INET;
  addr.sin_port   = htons(get_tcp_port());
  addr.sin_addr.s_addr = htonl(0x7f000001);

  TEST_FAIL_IF(connect(s, (struct sockaddr *)&addr, sizeof(addr)) != 0);
  TEST_FAIL_IF(shutdown(s, SHUT_WR) != 0);

  send(s, test_message, sizeof(test_message), 0);
}

/**
 * @testname shutdown_2
 * @testfor shutdown
 */
TEST_CASE(shutdown_2)
{
  int s = socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in addr;
  char buf[100];

  alarm(5);

  addr.sin_family = AF_INET;
  addr.sin_port   = htons(get_tcp_port());
  addr.sin_addr.s_addr = htonl(0x7f000001);

  TEST_FAIL_IF(connect(s, (struct sockaddr *)&addr, sizeof(addr)) != 0);
  TEST_FAIL_IF(shutdown(s, SHUT_RD) != 0);
  TEST(read(s, buf, sizeof(buf)) == 0);
}

/**
 * @testname shutdown_3
 * @testfor shutdown
 */
TEST_CASE_S(shutdown_3, SIGPIPE)
{
  int s = socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in addr;

  alarm(5);

  addr.sin_family = AF_INET;
  addr.sin_port   = htons(get_tcp_port());
  addr.sin_addr.s_addr = htonl(0x7f000001);

  TEST_FAIL_IF(connect(s, (struct sockaddr *)&addr, sizeof(addr)) != 0);
  TEST_FAIL_IF(shutdown(s, SHUT_RDWR) != 0);
  send(s, test_message, sizeof(test_message), 0);
}

/**
 * @testname shutdown_4
 * @testfor shutdown
 */
TEST_CASE(shutdown_4)
{
  int s = socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in addr;
  char buf[100];

  alarm(5);

  addr.sin_family = AF_INET;
  addr.sin_port   = htons(get_tcp_port());
  addr.sin_addr.s_addr = htonl(0x7f000001);

  TEST_FAIL_IF(connect(s, (struct sockaddr *)&addr, sizeof(addr)) != 0);
  TEST_FAIL_IF(shutdown(s, SHUT_RDWR) != 0);
  TEST(read(s, buf, sizeof(buf)) == 0);
}

/**
 * @testname sendmsg_1
 * @testfor sendmsg
 */
TEST_CASE(sendmsg_1)
{
  int sock =socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in addr;
  char buf[1024];
  int len;

  struct msghdr msg;
  struct iovec vec[3];

  TEST_FAIL_IF(sock == -1);
  alarm(5);

  addr.sin_family = AF_INET;
  addr.sin_port = htons(get_tcp_port());
  addr.sin_addr.s_addr = htonl(0x7F000001);
  TEST_FAIL_IF(connect(sock, (struct sockaddr *)&addr, sizeof addr) == -1);

  msg.msg_name = NULL;
  msg.msg_namelen = 0;
  msg.msg_iov = vec;
  msg.msg_iovlen = 3;
  msg.msg_control = NULL;
  msg.msg_controllen = 0;
  msg.msg_flags = 0;

  vec[0].iov_base = "One";
  vec[0].iov_len  = 3;
  vec[1].iov_base = "Two";
  vec[1].iov_len  = 0;
  vec[2].iov_base = "Three";
  vec[2].iov_len  = 3;

  TEST_FAIL_IF(sendmsg(sock, &msg, 0) != 6);
  TEST_FAIL_IF(read(sock, buf, sizeof buf) != 6);
  TEST_FAIL_IF(memcmp("OneThr", buf, 6) !=0);
  close(sock);
  TEST(1);
}

/**
 * @testname sendmsg_2
 * @testfor sendmsg
 */
TEST_CASE(sendmsg_2)
{
  int sock =socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in addr;

  struct msghdr msg;
  struct iovec vec[3];

  TEST_FAIL_IF(sock == -1);
  alarm(5);

  addr.sin_family = AF_INET;
  addr.sin_port = htons(get_tcp_port());
  addr.sin_addr.s_addr = htonl(0x7F000001);
  TEST_FAIL_IF(connect(sock, (struct sockaddr *)&addr, sizeof addr) == -1);

  msg.msg_name = NULL;
  msg.msg_namelen = 0;
  msg.msg_iov = vec;
  msg.msg_iovlen = (size_t)-1;
  msg.msg_control = NULL;
  msg.msg_controllen = 0;
  msg.msg_flags = 0;

  vec[0].iov_base = "One";
  vec[0].iov_len  = 3;
  vec[1].iov_base = "Two";
  vec[1].iov_len  = 0;
  vec[2].iov_base = "Three";
  vec[2].iov_len  = 3;

  TEST_FAIL_IF(sendmsg(sock, &msg, 0) != -1);
  close(sock);
  TEST(1);
}

/**
 * @testname sendmsg_3
 * @testfor sendmsg
 */
TEST_CASE(sendmsg_3)
{
  int sock =socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in addr;

  struct msghdr msg;
  struct iovec vec[2];

  TEST_FAIL_IF(sock == -1);
  alarm(5);

  addr.sin_family = AF_INET;
  addr.sin_port = htons(get_tcp_port());
  addr.sin_addr.s_addr = htonl(0x7F000001);
  TEST_FAIL_IF(connect(sock, (struct sockaddr *)&addr, sizeof addr) == -1);

  msg.msg_name = NULL;
  msg.msg_namelen = 0;
  msg.msg_iov = vec;
  msg.msg_iovlen = 2;
  msg.msg_control = NULL;
  msg.msg_controllen = 0;
  msg.msg_flags = 0;

  vec[0].iov_base = "not so long";
  vec[0].iov_len  = (ssize_t)((~(size_t)0) / 2);
  vec[1].iov_base = "trailing";
  vec[1].iov_len  = 1;

  TEST_FAIL_IF(sendmsg(sock, &msg, 0) != -1);
  close(sock);
  TEST(1);
}

/**
 * @testname recvmsg_1
 * @testfor recvmsg
 */
TEST_CASE(recvmsg_1)
{
  int sock =socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in addr;
  int len;

  struct msghdr msg;
  struct iovec vec[3];
  char buf[3][10];

  memset(buf, 'X', sizeof buf);

  TEST_FAIL_IF(sock == -1);
  alarm(5);

  addr.sin_family = AF_INET;
  addr.sin_port = htons(get_tcp_port());
  addr.sin_addr.s_addr = htonl(0x7F000001);
  TEST_FAIL_IF(connect(sock, (struct sockaddr *)&addr, sizeof addr) == -1);

  msg.msg_name = NULL;
  msg.msg_namelen = 0;
  msg.msg_iov = vec;
  msg.msg_iovlen = 3;
  msg.msg_control = NULL;
  msg.msg_controllen = 0;
  msg.msg_flags = 0;

  vec[0].iov_base = buf[0];
  vec[0].iov_len  = 3;
  vec[1].iov_base = buf[1];
  vec[1].iov_len  = 0;
  vec[2].iov_base = buf[2];
  vec[2].iov_len  = 5;

  TEST_FAIL_IF(write(sock, "OneTwoThree", sizeof "OneTwoThree") != sizeof "OneTwoThree");
  TEST_FAIL_IF(recvmsg(sock, &msg, 0) != 8);
  TEST_FAIL_IF(memcmp("OneXXXXXXX", vec[0].iov_base, 10) != 0);
  TEST_FAIL_IF(memcmp("XXXXXXXXXX", vec[1].iov_base, 10) != 0);
  TEST_FAIL_IF(memcmp("TwoThXXXXX", vec[2].iov_base, 10) != 0);
  close(sock);
  TEST(1);
}

/**
 * @testname recvmsg_2
 * @testfor recvmsg
 */
TEST_CASE(recvmsg_2)
{
  int sock =socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in addr;

  struct msghdr msg;
  struct iovec vec[3];

  TEST_FAIL_IF(sock == -1);
  alarm(5);

  addr.sin_family = AF_INET;
  addr.sin_port = htons(get_tcp_port());
  addr.sin_addr.s_addr = htonl(0x7F000001);
  TEST_FAIL_IF(connect(sock, (struct sockaddr *)&addr, sizeof addr) == -1);

  msg.msg_name = NULL;
  msg.msg_namelen = 0;
  msg.msg_iov = vec;
  msg.msg_iovlen = (size_t)-1;
  msg.msg_control = NULL;
  msg.msg_controllen = 0;
  msg.msg_flags = 0;

  vec[0].iov_base = "One";
  vec[0].iov_len  = 3;
  vec[1].iov_base = "Two";
  vec[1].iov_len  = 0;
  vec[2].iov_base = "Three";
  vec[2].iov_len  = 3;

  write(sock, "a", 1);
  TEST_FAIL_IF(recvmsg(sock, &msg, 0) != -1);
  close(sock);
  TEST(1);
}

/**
 * @testname recvmsg_3
 * @testfor recvmsg
 */
TEST_CASE(recvmsg_3)
{
  int sock =socket(AF_INET, SOCK_STREAM, 0);
  struct sockaddr_in addr;

  struct msghdr msg;
  struct iovec vec[2];

  TEST_FAIL_IF(sock == -1);
  alarm(5);

  addr.sin_family = AF_INET;
  addr.sin_port = htons(get_tcp_port());
  addr.sin_addr.s_addr = htonl(0x7F000001);
  TEST_FAIL_IF(connect(sock, (struct sockaddr *)&addr, sizeof addr) == -1);

  msg.msg_name = NULL;
  msg.msg_namelen = 0;
  msg.msg_iov = vec;
  msg.msg_iovlen = 2;
  msg.msg_control = NULL;
  msg.msg_controllen = 0;
  msg.msg_flags = 0;

  vec[0].iov_base = "not so long";
  vec[0].iov_len  = (ssize_t)((~(size_t)0) / 2);
  vec[1].iov_base = "trailing";
  vec[1].iov_len  = 1;

  write(sock, "a", 1);
  TEST_FAIL_IF(recvmsg(sock, &msg, 0) != -1);
  close(sock);
  TEST(1);
}
