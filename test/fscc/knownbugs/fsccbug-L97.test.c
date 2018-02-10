#include "common.h"

static void foo(void) {}

TEST_CASE(fsccbug_L97)
{
  if (foo) TEST(1);
  else TEST(0);
}
