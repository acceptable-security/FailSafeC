void test(void) {
  const int i = 1;
  int j = i;
  const int *p;

  p = &i;
  *p = j;
}
