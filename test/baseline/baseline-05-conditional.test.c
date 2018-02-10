#include <stdio.h>
#include "common.h"

/* test9-conditional.c */

static int test4(int x, int y, int z, int w) {
    return (x << 12) + (y << 8) + (z << 4) + w;
}

static int test(int x, int y) {
    return test4(x > y ? x + y : x, y > x ? y + x : y, x, y);
}

TEST_CASE(baseline_5_conditional_question)
{
    TEST(test(2, 3) == 0x2523);
    TEST(test(3, 3) == 0x3333);
    TEST(test(3, 2) == 0x5232);
}

/* test21-conditional.c */

static int i = 5;
static int j = 3;

TEST_CASE(baseline_5_conditional_andor)
{
    i++;
    j++;

    TEST(i > 5 && j > 3);
    TEST_FAIL_IF(i > 5 && j > 4);
    TEST_FAIL_IF(i > 6 && j > 3);
    TEST_FAIL_IF(i > 6 && j > 4);

    TEST(i > 5 || j > 3);
    TEST(i > 5 || j > 4);
    TEST(i > 6 || j > 3);
    TEST_FAIL_IF(i > 6 || j > 4);

    TEST(1 && 1);
    TEST_FAIL_IF(1 && 0);
    TEST(1 || 0);
    TEST(0 || 1);
    TEST_FAIL_IF(0 || 0);
}

/* test26-conditionals.c */
void y (void);

TEST_CASE(baseline_5_conditional_stmt)
{
    int i = 1;
    int j = 1;
    int f;
#define T (i == j)
#define F (i != j)

    if (T || T) ; else TEST_FAILED;
    if (T && T) ; else TEST_FAILED;
    if (T && F) TEST_FAILED; else ;
    if (F || F) TEST_FAILED; else ;

    if (T, F) TEST_FAILED; else ;
    if (F, T) ; else TEST_FAILED;

    f = 0;
    while(T) { f = 1; break; }
    TEST(f);
    
    f = 1;
    while(F) { f = 0; break; }
    TEST(f);
    
    f = 0;
    do { f = 1; break; } while(T);
    TEST(f);
    
    f = 0;
    do { f = 1; break; } while(F);
    TEST(f);
    
    f = 0;
    for(T;T;T) { f = 1; break; }
    TEST(f);
    
    f = 1;
    for(F;F;F) { f = 0; break; }
    TEST(f);
}
