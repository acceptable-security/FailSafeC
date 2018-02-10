#include "common.h"

struct S { int x, y; } g = { 1, 2 };

TEST_CASE(fsccbug_L103_1)
{
    struct S x = g;

    TEST(g.x == 1);
}
