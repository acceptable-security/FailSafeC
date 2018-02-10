#include <stdio.h>

int puts(const char *s) {
  int r;
  r = printf("%s\n", s);
  if (r < 0) return EOF;
  return 0;
}

int main(int argc, char *argv[]) {
  puts("Hello.");
  fputs(stdout, "Hello\n");
  return 0;
}
