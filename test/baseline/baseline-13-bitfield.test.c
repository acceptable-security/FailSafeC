#include <stdio.h>
#include "common.h"

/* test17-bitfield-{read,write}.c */

struct S {
    unsigned int x : 1;
    signed int y : 2;
    unsigned int z : 3;
};

static struct S x;

TEST_CASE(baseline_13_bitfield)
{
    struct S *p = &x;
    p->x = 1;
    p->y = 2;
    p->z = 3 + 16;

    TEST(p->x == 1);
    TEST(p->y == -2);
    TEST(p->z == 3);
}

/* test17-bitfield-init.c */

static struct T {
    int a;
    unsigned int x : 1;
    unsigned int y : 2;
    unsigned int z : 3;
    int b;
} a = { 20, 1, 2, 4, 30 };

TEST_CASE(baseline_13_bitfield_2)
{
    struct T b = { 40, 1, (int)&a, 5, 50 };
    
    TEST(a.a == 20);
    TEST(a.x == 1);
    TEST(a.y == 2);
    TEST(a.z == 4);
    TEST(a.b == 30);
    TEST(b.a == 40);
    TEST(b.x == 1);
    TEST(b.y == (((int)&a) & 3));
    TEST(b.z == 5);
    TEST(b.b == 50);
}
