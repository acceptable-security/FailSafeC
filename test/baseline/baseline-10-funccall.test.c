#include "common.h"

/* test20-funccall.c */

typedef int test_type;

static int t(int x) { return x + 1; }

int (*f2) (int) = t;

TEST_CASE(baseline_10_funccall)
{
    int (*f4) (int) = t;

    TEST(t(1) == 2);
    TEST(f2(1) == 2);
    TEST(f4(1) == 2);
}

typedef int Tfpcpc(char *, char *);
typedef Tfpcpc *Tpfpcpc;

TEST_CASE(baseline_10_funccall_undecl)
{
    Tpfpcpc t;

    char buf[5] = "a";
    baseline_10_funccall_undecl_testfunc(buf, "b");
    t = (Tpfpcpc)baseline_10_funccall_undecl_testfunc;
    t(buf, "c");
    TEST(buf[1] == 'b');
    TEST(buf[2] == 'c');
}
