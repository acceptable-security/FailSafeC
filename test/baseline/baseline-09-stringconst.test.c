#include "common.h"

/* test22-string-ptrinitializer.c */

static char *p1 = "123456";

TEST_CASE(baseline_9_stringconst_1)
{
    TEST(p1[0] == '1');
    TEST(p1[6] == 0);
}

/* test22-string-ptrconstant.c */

TEST_CASE(baseline_9_stringconst_2)
{
    char *p = "abc";

    TEST(p[0] == 'a');
    TEST(p[3] == 0);
}

/* test22-string-initializer.c */

static char s6[6] = "123456";
static char s7[7] = "123456";
static char s8[8] = "123456";
static char s0[] = "123456";
static char s00[] = "123456\n";

TEST_CASE(baseline_9_stringconst_3)
{
    TEST(sizeof(s6) == 6);
    TEST(sizeof(s7) == 7);
    TEST(sizeof(s8) == 8);
    TEST(sizeof(s0) == 7);
    TEST(sizeof(s00) == 8);
    TEST(s7[6] == 0);
    TEST(s8[6] == 0);
    TEST(s8[7] == 0);
    TEST(s0[6] == 0);
    TEST(s00[7] == 0);
}

static int f(char *p) {
    return p[3];
}

TEST_CASE(baseline_9_stringconst_4)
{
    TEST(f("abc") == 0);
}
