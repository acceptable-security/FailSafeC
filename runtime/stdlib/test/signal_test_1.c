#include <signal.h>
#include <stdio.h>
#include <stdlib.h>

void handler(int sig) {
  printf ("signal %d handled.\n", sig);
  exit(1);
}

void handler_cont(int sig) {
  printf ("signal %d handled. continue...\n", sig);
}

void test0() {
  struct sigaction sa, osa;

  sa.sa_handler = handler;
  sa.sa_flags = 0;
  
  sigaction(SIGINT, &sa, &osa);

  printf("old handler: %p\n", osa.sa_handler);

  sigaction(SIGINT, NULL, &osa);

  printf("old handler: %p\n", osa.sa_handler);

  printf("type Ctrl-C...");

  while(1) {};
}

void test1() {
  struct sigaction sa;

  sa.sa_handler = handler;
  sa.sa_flags = 0;

  sigaction(SIGSEGV, &sa, 0);

  printf("%d", *(int *)NULL);
}

void test2() {
  struct sigaction sa;

  sa.sa_handler = handler;
  sa.sa_flags = 0;

  sigaction(SIGSEGV, &sa, 0);

  raise(SIGSEGV);
}

void test3() {
  struct sigaction sa;

  sa.sa_handler = handler_cont;
  sa.sa_flags = SA_ONESHOT;

  sigaction(SIGSEGV, &sa, 0);

  printf ("1\n");
  raise(SIGSEGV);
  printf ("2\n");
  raise(SIGSEGV);
}

void broken_handler(int sig) {
  printf("%d", *(int *)NULL);
}

void test4() {
  struct sigaction sa;

  sa.sa_handler = broken_handler;
  sa.sa_flags = 0;

  sigaction(SIGSEGV, &sa, 0);

  raise(SIGSEGV);
}

void test5() {
  void (*h)(int);

  h = signal(SIGINT, handler);

  printf("old handler: %p\n", h);

  h = signal(SIGINT, handler);

  printf("old handler: %p\n", h);

  printf("type Ctrl-C...");

  while(1) {};
}

int main(int argc, char **argv) {
  if (argc == 1)
    test0();
  else if (*argv[1] == '1')
    test1();
  else if (*argv[1] == '2')
    test2();
  else if (*argv[1] == '3')
    test3();
  else if (*argv[1] == '4')
    test4();
  else if (*argv[1] == '5')
    test5();
  else
    printf("what test?\n");
  return 0;
}
