#include "common.h"

static char c;

TEST_CASE(baseline_0_intcond)
{
    int i;

    if (0)     i = 1; else i = 2; TEST(i == 2);
    if (i - 1) i = 1; else i = 2; TEST(i == 1);
    if (i - 1) i = 1; else i = 2; TEST(i == 2);
}

TEST_CASE(baseline_0_ptrcond)
{
    char *p = 0;
    int i;

    if (p)         i = 1; else i = 2; TEST(i == 2);
    if (&c)        i = 1; else i = 2; TEST(i == 1);
    if ((char *)0) i = 1; else i = 2; TEST(i == 2);
}
