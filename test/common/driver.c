/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <pty.h>
#include <sys/utsname.h>
#include <errno.h>

#ifdef SUFFIX
#define TEST_BIN "./test." SUFFIX
#else
#define TEST_BIN "./test"
#endif

struct test_entry {
  char *name;
  int signum;
} test_entries[] = {
#define TEST_CASE(func)      { #func, -1 },
#define TEST_CASE_S(func, s) { #func, s  },
#define USE_TEST_NOT_COMPILED
#ifndef FSC_ABRT
#define FSC_ABRT SIGABRT
#endif
#define GCC_ABRT -32
#include <testlist.c>
  { 0, -1}
};

static int test_server_udp_port = 20000;
static int test_server_tcp_port = 20000;

static int test_server_udp_socket;
static int test_server_tcp_socket;

static void add_env(char *name, int value)
{
  char *buf = (char*)malloc(256);
  sprintf(buf, "%s=%d", name, value);
  putenv(buf);
}

static void init_child(int pty_fd)
{
  int fd;
  system("rm -rf test_temp_dir");
  mkdir("test_temp_dir", 0755);
  remove("test_temp");
  remove("test_stdout");
  umask(0);
  close(1);
  fd = open("test_stdout", O_WRONLY | O_CREAT, 0644);
  add_env("TEST_PID", getpid());
  add_env("TEST_PPID", getppid());
  add_env("TEST_PGID", getpgid(0));
  add_env("TEST_UID", getuid());
  add_env("TEST_GID", getgid());
  add_env("TEST_EUID", geteuid());
  add_env("TEST_EGID", getegid());
  add_env("TEST_SID", getsid(0));
  add_env("TEST_PGRP", getpgrp());
  add_env("TEST_SERVER_UDP_PORT", test_server_udp_port);
  add_env("TEST_SERVER_TCP_PORT", test_server_tcp_port);
  add_env("TEST_PTY_FD", pty_fd);
  {
    char buf[L_cuserid];
    getlogin_r(buf, L_cuserid);
    setenv("TEST_LOGINNAME", buf, 1);
  }
  putenv("TEST_ENV=TEST");
  putenv("LC_ALL=C");
  putenv("TZ=UTC");
  putenv("DATEMSK=/tmp/fsc-test-datemsk");
  {
    struct utsname buf;
    uname(&buf);
    setenv("TEST_UNAME_SYSNAME", buf.sysname, 1);
    setenv("TEST_UNAME_NODENAME", buf.nodename, 1);
    setenv("TEST_UNAME_RELEASE", buf.release, 1);
    setenv("TEST_UNAME_VERSION", buf.version, 1);
    setenv("TEST_UNAME_MACHINE", buf.machine, 1);
  }
}

static void alarm_handler(int num)
{
  fprintf(stderr, "detect infinite loop!\n");
}

static void run_test(struct test_entry *entry)
{
  pid_t pid;
  int master, slave;
  
  fprintf(stdout, "Test %s: ", entry->name);
  fprintf(stderr, "Test %s: ", entry->name);
  fflush(stdout);
  fflush(stderr);

  openpty(&master, &slave, NULL, NULL, NULL);
  
  pid = fork();
  if (pid == 0) {
    close(master);
    init_child(slave);
    execl(TEST_BIN, TEST_BIN, entry->name, (char *)0);
    perror("execl " TEST_BIN);
    _exit(127);
  } else if (pid > 0) {
    int status;
    int success = 0;
    int indistinguishable = 0;
    struct sigaction sa;

    sa.sa_handler = alarm_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;

    sigaction(SIGALRM, &sa, NULL);
    alarm(20);
    while (wait(&status) != pid) {
      perror("wait");
      if (errno != EINTR) {
        exit(127);
      }
      kill(pid, SIGKILL);
    }
    close(slave);
    close(master);
    if (WIFEXITED(status) && WEXITSTATUS(status) == 0 && entry->signum == -1) {
      success = 1;
    } else {
      if (entry->signum == GCC_ABRT) {
        if (WIFSIGNALED(status) && (WTERMSIG(status) == SIGABRT || WTERMSIG(status) == SIGSEGV)) {
          success = 1;
        } else {
          indistinguishable = 1;
        }
      } else if (entry->signum >= 0) {
        if (WIFSIGNALED(status) && WTERMSIG(status) == entry->signum) {
          success = 1;
        }
      }
    }
    if (!success) {
      if (WIFEXITED(status)) {
        fprintf(stderr, "exited(%d)\n", WEXITSTATUS(status));
      } else if (WIFSIGNALED(status)) {
        fprintf(stderr, "signaled(%d)\n", WTERMSIG(status));
      }
    }
    {
      char *msg = indistinguishable? "Unknown": success? "OK": "Failed";
      fprintf(stdout, "%s\n", msg);
      fprintf(stderr, "%s\n", msg);
      fflush(stdout);
    }
  } else {
    perror("fork");
    exit(127);
  }
}

static void bind_socket(int udp)
{
  int sock = socket(PF_INET, udp? SOCK_DGRAM: SOCK_STREAM, 0);
  struct sockaddr_in addr;
  int i;

  addr.sin_family = AF_INET;
  addr.sin_addr.s_addr = htonl(INADDR_ANY);

  for (i = 0;; i++) {
    addr.sin_port = htons(udp? test_server_udp_port: test_server_tcp_port);
    if (bind(sock, (struct sockaddr *)&addr, sizeof addr) == 0) {
      break;
    }
    udp? test_server_udp_port++: test_server_tcp_port++;
  }
  if (udp)
    test_server_udp_socket = sock;
  else
    test_server_tcp_socket = sock;
}

static void test_server_tcp()
{
  int s;
  
  listen(test_server_tcp_socket, 1);
  while ((s = accept(test_server_tcp_socket, NULL, NULL)) >= 0) {
    char buf[1024];
    int len;

    while ((len = recv(s, buf, sizeof buf, 0)) > 0) {
      send(s, buf, len, 0);
    }
  }
}

static void test_server_udp()
{
  int sock = test_server_udp_socket;
  char buf[1024];
  int len;
  struct sockaddr_in addr;
  int addr_len;

  for (;;) {
    addr_len = sizeof addr;
    len = recvfrom(sock, buf, sizeof buf, 0, (struct sockaddr *)&addr, &addr_len);
    if (len > 0) {
      sendto(sock, buf, len, 0, (struct sockaddr *)&addr, addr_len);
    }
  }
}

static int fork_test_server(int udp)
{
  int pid = fork();

  if (pid < 0) {
    perror("fork");
    exit(1);
  } else if (pid == 0) {
    if (udp) {
      test_server_udp();
    } else {
      test_server_tcp();
    }
    fprintf(stderr, "server %s exiting\n", udp? "udp": "tcp");
    _exit(25);
  }
  return pid;
}

int main(void)
{
  int i, j;
  int pids[2];

  bind_socket(0);
  bind_socket(1);
  fprintf(stderr, "port tcp=%d, udp=%d\n", test_server_tcp_port, test_server_udp_port);
  pids[0] = fork_test_server(0);
  pids[1] = fork_test_server(1);
  
  for (i = 0; test_entries[i].name; i++) {
    run_test(&test_entries[i]);
  }
  kill(pids[0], SIGTERM);
  kill(pids[1], SIGTERM);
  return 0;
}

