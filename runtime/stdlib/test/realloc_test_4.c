#include <stdlib.h>
#include <stdio.h>

struct S {
  int x;
  int y;
  char z;
};

struct S a;

void test(void *ti_p, int s1, int s2) {
  typedef int typ;
  int i;
  typ *q;
  typ *p = (typ *)malloc_typed(s1 * sizeof(typ), ti_p);

  for(i = 0; i < s1; i++) {
    p[i] = i;
  }

  q = realloc(p, s2 * sizeof(typ));

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
  test(__typeof(long long), 3, 5);
  test(__typeof(long long), 5, 3);
  test(__typeof(struct S), 4, 7);
  test(__typeof(struct S), 7, 4);
  test(__typeof(int), 0, 12);
  test(__typeof(int), 12, 0);
  return 0;
}
