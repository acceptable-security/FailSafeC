#include "common.h"

struct st {
  int x;
};

static struct st foo(void) {
  struct st s = { 5 };
  return s;
}

TEST_CASE(fsccbug_L114_1)
{
  struct st x = { 0 };
  struct st (*funcp)(void) = foo;
  x = funcp();
  TEST(x.x == 5);
}

TEST_CASE(fsccbug_L114_2)
{
  struct st x = { 0 };
  struct st (*funcp)(int) = (struct st (*)(int))foo;
  x = funcp(1);
  TEST(x.x == 5);
}
