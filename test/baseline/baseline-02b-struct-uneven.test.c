#include "common.h"

static struct S {
  int a;
  float b;
  char c;
} x[2];

TEST_CASE(baseline_2b_struct_uneven)
{
    struct S *p = x;
    p--;
    p++;
    TEST(p == x);
}
