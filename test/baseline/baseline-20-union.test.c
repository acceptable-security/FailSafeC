#include "common.h"

struct S1 {
    int x;
    float y;
    int z;
};

struct S2 {
    int x;
    char *y;
    int z;
};

union U1 {
    struct S1 s1;
    struct S2 s2;
};

union U2 {
    struct S2 s2;
    struct S1 s1;
};

static union U1 u1 = { 1, 1.0, 10 };
static union U2 u2 = { 2, "2", 20 };

TEST_CASE(baseline_20_union_1)
{
    TEST(u1.s1.x == 1);
    TEST(u1.s2.x == 1);
    TEST(u2.s1.x == 2);
    TEST(u2.s2.x == 2);
    TEST(u1.s1.z == 10);
    TEST(u1.s2.z == 10);
    TEST(u2.s1.z == 20);
    TEST(u2.s2.z == 20);
}
