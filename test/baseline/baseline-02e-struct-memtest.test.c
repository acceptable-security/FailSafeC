#include "common.h"
#include <libfsc/libfsc.h>

static struct S1 {
    struct { short s[5]; } sas[5000];
    int i;
} x1;

static struct S2 {
    struct { char s[5]; } sas[5];
    int i;
} x2;

static struct S3 {
    int i[1];
} x3;

static struct S4 {
    long long ll;
    double d;
    float f;
    int i;
    short s;
    char c;
} x4;

static struct S5 {
    char c;
    char a[23];
    int x[2];
} x5;

TEST_CASE(baseline_2e_struct_memtest)
{
    TEST(fsc_test_memory_block(&x1, 1));
    TEST(fsc_test_memory_block(&x2, 1));
    TEST(fsc_test_memory_block(&x3, 1));
    TEST(fsc_test_memory_block(&x4, 1));
    TEST(fsc_test_memory_block(&x5, 1));
}

