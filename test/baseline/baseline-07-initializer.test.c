#include "common.h"

/* test13-initializer-ok.c */

static short a1[5] = { 17 };
static int j1 = 3 * 5 + (int)a1;
static int i1 = (int)&j1;
static short *p1 = &a1[3];

TEST_CASE(baseline_7_initializer_1)
{
    int a2[2] = { 1, (int)&j1 };
    int k2 = 5 + (int)&i1;

    TEST(a2[0] == 1);
    TEST(a2[1] == (int)&j1);
    TEST(k2 - (int)&i1 == 5);
    TEST(a1[0] == 17);
    TEST(a1[1] == 0);
    TEST(j1 - (int)&a1 == 15);
    TEST(i1 == (int)&j1);
    TEST(p1 - 3 == a1);
}

