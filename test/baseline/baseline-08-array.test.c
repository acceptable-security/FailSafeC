#include "common.h"

/* test8-0-array-basic.c */

TEST_CASE(baseline_8_array_basic)
{
  int a[80] = { 1 };

  int *pi, (*pai)[80];

  pai = &a;
  pi = a;
  pi = *pai;

  TEST((*pai)[0] == 1);
}

/* test13-initializer-array.c */

static int a[10] = { 1, 2, 3, 4, 5 };
static int b[] = { 1, 2, 3, 4, 5 };

TEST_CASE(baseline_8_array_global_init)
{
    TEST(sizeof(a) / sizeof(int) == 10);
    TEST(sizeof(b) / sizeof(int) == 5);
    TEST(a[0] == 1);
    TEST(a[1] == 2);
    TEST(a[2] == 3);
    TEST(a[3] == 4);
    TEST(a[4] == 5);
    TEST(a[5] == 0);
    TEST(a[6] == 0);
    TEST(a[7] == 0);
    TEST(a[8] == 0);
    TEST(a[9] == 0);
    TEST(b[0] == 1);
    TEST(b[1] == 2);
    TEST(b[2] == 3);
    TEST(b[3] == 4);
    TEST(b[4] == 5);
}

/* test13-initializer-ok-simple.c */

TEST_CASE(baseline_8_array_local_init)
{
    static int c[10] = { 1, 2, 3, 4, (int)&c };
    static int d[] = { 1, 2, 3, 4, 5 };

    TEST(sizeof(c) / sizeof(int) == 10);
    TEST(sizeof(d) / sizeof(int) == 5);
    TEST(c[0] == 1);
    TEST(c[1] == 2);
    TEST(c[2] == 3);
    TEST(c[3] == 4);
    TEST(c[4] == (int)&c);
    TEST(c[5] == 0);
    TEST(c[6] == 0);
    TEST(c[7] == 0);
    TEST(c[8] == 0);
    TEST(c[9] == 0);
    TEST(d[0] == 1);
    TEST(d[1] == 2);
    TEST(d[2] == 3);
    TEST(d[3] == 4);
    TEST(d[4] == 5);
}

/* test5-mdreduce.c */

static int e[3][3] = {{1,2,3},{4,5,6},{7,8,9}};
TEST_CASE(baseline_8_array_mdarray)
{
    TEST(sizeof(e) / sizeof(int) == 9);
    TEST(e[0][0] == 1);
    TEST(e[0][1] == 2);
    TEST(e[0][2] == 3);
    TEST(e[1][0] == 4);
    TEST(e[1][1] == 5);
    TEST(e[1][2] == 6);
    TEST(e[2][0] == 7);
    TEST(e[2][1] == 8);
    TEST(e[2][2] == 9);
    TEST(e[0][6] == 7);
    TEST(e[0][7] == 8);
    TEST(e[0][8] == 9);
}
