#include "common.h"

static int foo(void)
{
  int x = 24;
  switch (0) {
  case 0: do x++; while (0);
  }
  return x;
}

TEST_CASE(fsccbug_L93_1)
{
  TEST(foo() == 25);
}
