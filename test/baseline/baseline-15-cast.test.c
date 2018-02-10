#include "common.h"

/* test15-cast.c, test15-cast2.c */

static int x;
static char *y;
static int *z;
static char c;

TEST_CASE(baseline_15_cast_1)
{
    x = 5;
    y = (char *)x;
    TEST((int)y == 5);
    return;
}

TEST_CASE(baseline_15_cast_2)
{
    y = &c;
    x = (int)y;
    TEST((char *)x == &c);
    return;
}

TEST_CASE(baseline_15_cast_3)
{
    z = &x;
    y = (char *)z;
    TEST((int *)y == &x);
    y = &c;
    z = (int *)y;
    TEST((char *)z == &c);
    return;
}
