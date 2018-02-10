#include <stdio.h>

int main(int argc, char *argv[])
{
  int x = 24;
  switch (0) {
  case 0: do x++; while (0);
  }
  printf("%d\n", x);
  return 0;
}
