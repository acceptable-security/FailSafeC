#include "common.h"

TEST_CASE(fsccbug_L108)
{
    double d1 = 1.5f;
    double d2 = 1.5;

    TEST(d1 * 2.0 == 3.0);
    TEST(d2 * 2.0 == 3.0);
    TEST(1.23F != (float)d2);
}
