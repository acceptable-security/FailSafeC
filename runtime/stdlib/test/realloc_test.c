#include <stdlib.h>
#include <stdio.h>

void test(void *t, int s1, int s2) {
  int i;
  char *q;
  char *p = (char *)malloc_typed(s1, t);

  for(i = 0; i < s1; i++) {
    p[i] = i;
  }

  q = realloc(p, s2);

  printf("%p -> %p: ", p, q);
  for(i = 0; i < s2; i++) {
    printf("%d ", q[i]);
  }
  printf("\n");
  free(q);
}

int main(int argc, char **argv) {
  test(__typeof(char), 5, 10);
  test(__typeof(char), 10, 5);
  test(__typeof(int), 6, 12);
  test(__typeof(int), 12, 6);
  test(__typeof(int), 0, 12);
  test(__typeof(int), 12, 0);
  return 0;
}
