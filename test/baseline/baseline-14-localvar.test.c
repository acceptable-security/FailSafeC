#include "common.h"

/* test16-localvariable-single.c */

static int test_ext(int *a) { return *a + (int)a; }

TEST_CASE(baseline_14_localvar_1)
{
    int local = 1;
    TEST(test_ext(&local) == 1 + (int)&local);
}

/* test16-localvariable-array.c */

TEST_CASE(baseline_14_localvar_2)
{
    int local[1] = {2};
    TEST(test_ext(local) == 2 + (int)&local);
}

TEST_CASE(baseline_14_localvar_3)
{
    int local[2] = {3,4};
    TEST(test_ext(&local[0]) == 3 + (int)&local);
    TEST(test_ext(&local[1]) == 4 + sizeof(int) + (int)&local);
}

/* test11-escaping.c */

static int global = 17;

static int test2(int *p)
{
    return *p;
}

static int test(int a)
{
    return test2(&a);
}

TEST_CASE(baseline_14_localvar_escape)
{
    int local = 26;

    TEST(test2(&global) == global);
    global = 49;
    TEST(test2(&global) == global);
    TEST(test2(&local) == local);
    local = 31;
    TEST(test2(&local) == local);
    TEST(test(54) == 54);
}
