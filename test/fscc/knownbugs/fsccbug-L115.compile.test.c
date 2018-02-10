#include <stdio.h>

int foo(int a) {
  const int b = a > 0 ? 1 : 0;
  return b;
}

int main(int argc, char *argv[]) {
  foo(1);
  foo(0);
  return 0;
}
