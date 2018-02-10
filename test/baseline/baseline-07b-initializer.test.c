#include "common.h"
#include <stdio.h>

/* test13-initializer-ok.c */

static int x = 0;

static int a1[5] = {
    -((int)&(((int *)0)[1])),
};

/*
            *    *    *   **
     +VV, +PV, -VV, -PV, -PP.
II    OK   OK   OK   OK   OK
IP    OK   ng   ng   ng   ng
PI    OK   OK   OK   OK   ng
PP    ng   ng   OK=  ng   OK=

 */

static long a2[] = {
    1 + 2,
    (int)((int*)8 + 1),
    3 - 2,
    (int)((int*)8 - 1),
    (int)((int*)12 - (int*)4),

    1 + (int)&x,

    (int)&x + 1,
    (int)(&x + 1),
    (int)(&x + 2) - 1,
    (int)((&x + 2) - 1),

    (int)(&x + 2) - (int)(&x + 1),
    (&x + 2) - (&x + 1)
};

TEST_CASE(baseline_7_initializer_2)
{
    TEST(a1[0] == -(sizeof(int)));

    TEST(a2[0] == 3);
    TEST(a2[1] == 12);
    TEST(a2[2] == 1);
    TEST(a2[3] == 4);
    TEST(a2[4] == 2);

    TEST(a2[5] == 1 + (int)&x);

    TEST(a2[6] == 1 + (int)&x);
    TEST(a2[7] == 4 + (int)&x);
    TEST(a2[8] == 7 + (int)&x);
    TEST(a2[9] == 4 + (int)&x);

    TEST(a2[10] == 4);
    TEST(a2[11] == 1);
}

