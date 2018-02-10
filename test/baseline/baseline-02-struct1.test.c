#include "common.h"

/* test14-field-1.c */

static struct S1 {
  int i;
  char c;
} s;

TEST_CASE(baseline_2_struct1_1)
{
    TEST(sizeof(s) == sizeof(int) * 2);
    s.i = 5;
    TEST(s.i == 5);
    s.c = 3;
    TEST(s.c == 3);
}

TEST_CASE(baseline_2_struct1_2)
{
    struct S1 *p = &s;
    p->i = 52;
    TEST(p->i == 52);
    TEST(s.i == 52);
    p->c = 32;
    TEST(p->c == 32);
    TEST(s.c == 32);
}

static struct T1 {
    struct S1 s;
} t;

TEST_CASE(baseline_2_struct1_3)
{
    struct T1 *p = &t;
    t.s.i = 5;
    TEST(t.s.i == 5);
    t.s.c = 3;
    TEST(t.s.c == 3);
    p->s.i = 52;
    TEST(p->s.i == 52);
    TEST(t.s.i == 52);
    p->s.c = 32;
    TEST(p->s.c == 32);
    TEST(t.s.c == 32);
}

/* test14-field-2amp.c */

TEST_CASE(baseline_2_struct1_4)
{
    struct S1 *p = &s;
    char *pc1 = &s.c;
    char *pc2 = &p->c;
    TEST((int)pc1 == sizeof(int) + (int)&s);
    TEST((int)pc2 == sizeof(int) + (int)&s);
}

/* test14-field-3.c */

static struct S1 s2 = { 3, 'c' };

TEST_CASE(baseline_2_struct1_5)
{
    TEST(s2.i == 3);
    TEST(s2.c == 'c');
}

/* test13-initializer-struct.c */

TEST_CASE(baseline_2_struct1_6)
{
    struct S1 b = { (int)&b, 'd' };
    TEST(b.i == (int)&b);
    TEST(b.c == 'd');
}

/* test6-field.c */

static struct S2 { struct T2 *p; int x; };

static struct T2 { char c[5]; struct S2 s; } t2;

TEST_CASE(baseline_2_struct1_7)
{
    t2.s.p = &t2;
    t2.s.x = 5;
    TEST(t2.s.p->s.x = 5);
}

/* test31-mdarray.c */

static struct S3 {
    int i;
    int a[16][16];
} s3;

TEST_CASE(baseline_2_struct1_8)
{
    s3.a[1][3] = 5;
    TEST(s3.a[1][3] == 5);
}

/* test13-initializer-struct-easy.c: */

static struct S4 {
  int x;
  double y;
} a4 = { 17, 2.0 };

static struct S4 c4[5] = { 17, 2.0, 30, 4.0 };

TEST_CASE(baseline_2_struct1_9)
{
    TEST(a4.x == 17);
    TEST(a4.y == 2.0);
    TEST(c4[1].x == 30);
    TEST(c4[2].x == 0);
}
