#include <stdarg.h>
#include <stdio.h>
#include "common.h"

/* test24-varargs.c */

static int testf(char *p, ...) {
    va_list ap;
    int i = 0, j;

    va_start(ap, p);
    while((j = va_arg(ap, int)) != 0)
	i += j;

    va_end(ap);
    return i;
}

static char *s = "abc";

TEST_CASE(baseline_17_vararg)
{
    int i = (int)s;
    char *one = &s[-i + 1];

    TEST(testf(s, s, 0) == i);
    TEST(testf(s, i, 0) == i);
    TEST(testf(s, 1, 0) == 1);
    TEST(testf(s, one, 0) == 1);
    TEST(testf(s, i, one, 2, 3, 0x400000005LL, 0) == i + 15);
}
