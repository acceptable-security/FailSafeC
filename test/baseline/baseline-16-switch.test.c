#include "common.h"

static int test(int x) {
    switch (x) {
    case 1:
    case 2:
	return 1;
    case 3:
	return 2;
    case 4:
	return 3;
    case -1:
	test(1);
    case 5:
	return 5;
    default:
	return 7;
    }
}

static int test2(int x) {
    switch (x) {
    case 1:
    case 2:
	return 1;
    case 3:
	return 2;
    case 4:
	return 3;
    case 5:
	return 5;
    case -1:
	test(1);
    }
    return 7;
}

TEST_CASE(baseline_16_switch)
{
    TEST(test(1) == 1);
    TEST(test(2) == 1);
    TEST(test(3) == 2);
    TEST(test(-1) == 5);
    TEST(test2(-1) == 7);
    TEST(test(0) == 7);
    TEST(test(6) == 7);
    TEST(test2(0) == 7);
    TEST(test2(6) == 7);
}
