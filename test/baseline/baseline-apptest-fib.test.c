#include "common.h"
static int fib (int x) {
  if (x < 2)
    return 1;
  else
    return fib(x - 1) + fib(x - 2);
}

TEST_CASE(baseline_apptest_fib)
{
    TEST(fib(29) == 832040);
}
