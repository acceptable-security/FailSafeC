#include "common.h"

TEST_CASE(baseline_25b_loops)
{
    int a = 1, b = 2, i, t;

    for (i = 0; i < 3; i++) {
	t = a;
	a = b;
	b = t;
    }

    TEST(a == 2);
}
