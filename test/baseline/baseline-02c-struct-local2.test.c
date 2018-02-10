#include <stdlib.h>
#include "common.h"

/* test35-localstruct.c */
struct A {
  char *a;
  int b;
};

TEST_CASE(baseline_2_struct1_localinit_1)
{
  struct A x = { "abc", 2 };

  TEST(x.a[0] == 'a');
  TEST(x.b == 2);
}

TEST_CASE(baseline_2_struct1_localinit_2)
{
    struct A x[2] = {"abc", 1, "cde", 2};

    TEST(x[0].a[0] == 'a');
    TEST(x[0].b == 1);
    TEST(x[1].a[0] == 'c');
    TEST(x[1].b == 2);
}

struct B {
    int a[2];
};

TEST_CASE(baseline_2_struct1_localinit_3)
{
    struct B x[2] = { 1,2,3,4 };
    TEST(x[0].a[0] == 1);
    TEST(x[0].a[1] == 2);
    TEST(x[1].a[0] == 3);
    TEST(x[1].a[1] == 4);
}

/* test5-field.c */
struct S1 { int x; };

static struct S1 f(void) {
    struct S1 s = { 1 };
    return s;
}

TEST_CASE(baseline_2_struct1_func_return)
{
    TEST(f().x == 1);
}

/* test8-array-in-struct.c */

struct S2 {
    char buf[80];
    int matrix[5][5];
};

TEST_CASE(baseline_2_struct1_array_member_access)
{
    struct S2 s = {0};

    s.buf[2] = 'd';
    s.matrix[3][4] += 3;

    TEST(s.buf[2] == 'd');
    TEST(s.matrix[3][4] == 3);
}

/* test2.c */

struct S3 { int x; int a[5][4][3]; };

TEST_CASE(baseline_2_struct1_array_member_addr)
{
  struct S3 x = { 9, {{1, 2}} };

  int (*c)[4][3] = &x.a[2];

  TEST(1);
}

