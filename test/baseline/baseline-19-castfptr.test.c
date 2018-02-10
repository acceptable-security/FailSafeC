#include "common.h"

static int test(int *a, int *b) {
    int x;
    x = *a - *b;
    return x;
}

static int testq(int (*f)(void *, void *), void *a, void *b) {
    int r;
    r = f(a, b);
    return r;
}

TEST_CASE(baseline_19_castfptr)
{
    int a = 1;
    int b = 2;
    int r = testq((int (*)(void *, void*))test, &a, &b);
    TEST(r == -1);
}
