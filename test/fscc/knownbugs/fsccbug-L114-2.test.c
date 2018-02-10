#include "common.h"

struct st {
  int x;
};

static int foo(struct st v) {
  return v.x;
}

TEST_CASE(fsccbug_L114b_1)
{
  struct st x = { 5 };
  TEST(foo(x) == 5);
}

TEST_CASE(fsccbug_L114b_2)
{
  struct st x = { 5 };
  int (*funcp)(struct st, int) = (int (*)(struct st, int))foo;
  TEST(funcp(x, 1) == 5);
}
