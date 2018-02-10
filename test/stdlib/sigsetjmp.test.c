#include "common.h"
#include <setjmp.h>
#include <stdio.h>

TEST_CASE(sigsetjmp_1)
{
    sigjmp_buf b;
    int r;

    r = sigsetjmp(b, 0);
    TEST(1);
}

TEST_CASE(sigsetjmp_2)
{
    sigjmp_buf b;
    int r = 7;

    if ((r = sigsetjmp(b, 0)) != 5) {
	siglongjmp(b, r + 1);
	TEST(0);
    }

    TEST(r == 5);
}

static sigjmp_buf b, bb;

static void test_sigsetjmp_1(int i) {
    sigsetjmp(bb, 0);
    if (i > 100) {
	siglongjmp(b, i);
	TEST(0);
    } else
	test_sigsetjmp_1(i * 2);
    TEST(0);
}

static int test_sigsetjmp_2(int i) {
    int r = 5;

    if (r = sigsetjmp(b, 0)) {
	if (r == 160) siglongjmp(bb, 5);
	if (r == 180) siglongjmp(b, 0); /* turned to 1 by siglongjmp */
	if (r == 200) {
	    *(int *)(&b[0]) = 0;
            siglongjmp(b, 0);
        }
	return r;
    }

    test_sigsetjmp_1(i);

    return 0;
}

TEST_CASE(sigsetjmp_3_0)
{
    TEST(test_sigsetjmp_2(1) == 128);
    TEST(test_sigsetjmp_2(6) == 192);
    TEST(test_sigsetjmp_2(45) == 1); /* trapped by 180, reentry */
}

TEST_CASE_S(sigsetjmp_3_1, FSC_ABRT)
{
    test_sigsetjmp_2(5);
    TEST(1);
}

TEST_CASE_S(sigsetjmp_3_2, FSC_ABRT)
{
    test_sigsetjmp_2(25);
    TEST(1);
}

TEST_CASE_S(sigsetjmp_4, FSC_ABRT)
{
    sigjmp_buf b;
    int r = 7;

    if ((r = sigsetjmp(b, 0)) != 5) {
	longjmp(b, r + 1);
    }

    TEST(1);
}

TEST_CASE(sigsetjmp_5)
{
  int ret;
  volatile int flag = 0;
  sigjmp_buf buf1, buf2;

  ret = sigsetjmp(buf1, 0);
  if (ret == 0) {
    flag |= 1;
    memcpy(buf2, buf1, sizeof(buf1));
    siglongjmp(buf2, 1);
  } else {
    flag |= 2;
  }
  TEST(flag == 3);
}
