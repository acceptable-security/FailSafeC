#include "common.h"

static int test(void) {
  static y = 5;
  static y2 = 5;

  static *z = &y;
  y++;
  return y;
}

TEST_CASE(baseline_3_local_static)
{
    test();
    test();
    TEST(test() == 8);
}
