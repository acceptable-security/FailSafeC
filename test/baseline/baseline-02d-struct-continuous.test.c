#include "common.h"
#include <libfsc/libfsc.h>

#define TI_CONTINUOUS 0x20

/* 2,3,4 should be continous, 1,5 should not */
static struct S1 {
    int i;
} s1;

static struct S2 {
    char c;
} s2;

static struct S3 {
    char c;
    float f;
} s3;

static struct S4 {
    char c[5];
    struct S2 s[5];
} s4;

static struct S5 {
    struct S1 s1;
    struct S2 s2;
} s5;

TEST_CASE(baseline_2d_struct_continuous)
{
    TEST(!(TI_CONTINUOUS & fsc_flags_of_type(__typeof(struct S1))));
    TEST( (TI_CONTINUOUS & fsc_flags_of_type(__typeof(struct S2))));
    TEST( (TI_CONTINUOUS & fsc_flags_of_type(__typeof(struct S3))));
    TEST( (TI_CONTINUOUS & fsc_flags_of_type(__typeof(struct S4))));
    TEST(!(TI_CONTINUOUS & fsc_flags_of_type(__typeof(struct S5))));
}
