/* test12-gvar.c */

#include "common.h"

typedef int testtype;

testtype cx = 1;
static testtype cy = 2;
testtype *cp = &cy;

testtype test12 (void) {
    static testtype cz;
    cy = cx;
    return cx + cy + cz + *cp;
}

TEST_CASE(baseline_6_globalvar)
{
    TEST(test12() == 3);
}

/* test13-initializer-global.c */
static int a[5] = { 10, 11, 12, 13 };
static int j = 3 * 5 + (int)a;
static int i = (int)&j;
static int *p = &a[3];

TEST_CASE(baseline_6_globalvar_2)
{
    TEST(a[0] == 10);
    TEST(a[1] == 11);
    TEST(a[2] == 12);
    TEST(a[3] == 13);
    TEST(a[4] == 0);
    TEST(j - 15 == (int)a);
    TEST(*((int *)i) == j);
    TEST(*p == 13);
}
