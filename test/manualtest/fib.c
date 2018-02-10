#include <stdio.h>
#include <stdlib.h>

static int fib (int x) {
  if (x < 2)
    return 1;
  else
    return fib(x - 1) + fib(x - 2);
}

int main(int argc, char **argv) {
    int x = 5;
    if (argc >= 2)
	x = atoi(argv[1]);
    printf("fib(%d) = %d\n", x, fib(x));
    return 0;
}
