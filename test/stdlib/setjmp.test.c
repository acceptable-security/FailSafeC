#include "common.h"
#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

TEST_CASE(setjmp_1)
{
    jmp_buf b;
    int r;

    r = setjmp(b);
    TEST(1);
}

TEST_CASE(setjmp_2)
{
    jmp_buf b;
    int r = 7;

    if ((r = setjmp(b)) != 5) {
	longjmp(b, r + 1);
	TEST(0);
    }

    TEST(r == 5);
}

static jmp_buf b, bb;

static void test_setjmp_1(int i) {
    setjmp(bb);
    if (i > 100) {
	longjmp(b, i);
	TEST(0);
    } else
	test_setjmp_1(i * 2);
    TEST(0);
}

static int test_setjmp_2(int i) {
    int r = 5;

    if (r = setjmp(b)) {
	if (r == 160) longjmp(bb, 5); /* invoking stale stack frame */
	if (r == 180) longjmp(b, 0); /* turned to 0 by longjmp */
	if (r == 200) {
	    *(int*)(&b[0]) = 0;
            longjmp(b, 0);
        }
	return r;
    }

    test_setjmp_1(i);

    return 0;
}

TEST_CASE(setjmp_3_0)
{
    TEST(test_setjmp_2(1) == 128);
    TEST(test_setjmp_2(6) == 192);
    TEST(test_setjmp_2(45) == 1); /* trapped by 180, reentry */
}

TEST_CASE_S(setjmp_3_1, FSC_ABRT)
{
    test_setjmp_2(5);
    TEST(1);
}

TEST_CASE_S(setjmp_3_2, FSC_ABRT)
{
    test_setjmp_2(25);
    TEST(1);
}

TEST_CASE_S(setjmp_4, FSC_ABRT)
{
    jmp_buf b;
    int r = 7;

    if ((r = setjmp(b)) != 5) {
	siglongjmp(b, r + 1);
    }

    TEST(1);
}

TEST_CASE(setjmp_5)
{
  int ret;
  volatile int flag = 0;
  jmp_buf buf1, buf2;

  ret = setjmp(buf1);
  if (ret == 0) {
    flag |= 1;
    memcpy(buf2, buf1, sizeof(buf1));
    longjmp(buf2, 1);
  } else {
    flag |= 2;
  }
  TEST(flag == 3);
}
