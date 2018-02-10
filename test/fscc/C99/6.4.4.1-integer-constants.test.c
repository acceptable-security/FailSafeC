/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"

TEST_CASE(C99_6_4_4_1_integer_constants_1)
{
  TEST_FAIL_IF(0 != 00);
  TEST_FAIL_IF(1023 != 01777);
  TEST_FAIL_IF(1023 != 01777U);
  TEST_FAIL_IF(1023 != 01777u);
  TEST_FAIL_IF(1023 != 01777L);
  TEST_FAIL_IF(1023 != 01777l);
  TEST_FAIL_IF(1023 != 01777LL);
  TEST_FAIL_IF(1023 != 01777ll);
  TEST_FAIL_IF(1023 != 01777ul);
  TEST_FAIL_IF(1023 != 01777lu);
  TEST_FAIL_IF(1023 != 01777ull);
  TEST_FAIL_IF(1023 != 01777ULL);
  TEST_FAIL_IF(1023 != 0x3FF);
  TEST_FAIL_IF(1023 != 0x3ff);
  TEST_FAIL_IF(1023 != 0x3fF);
  TEST_FAIL_IF(1023 != 0x3Ff);
  TEST_FAIL_IF(1023 != 0x3FFu);
  TEST_FAIL_IF(1023 != 0x3FFU);
  TEST_FAIL_IF(1023 != 0x3FFl);
  TEST_FAIL_IF(1023 != 0x3FFL);
  TEST_FAIL_IF(1023 != 0x3FFll);
  TEST_FAIL_IF(1023 != 0x3FFLL);
  TEST_FAIL_IF(1023 != 0x3FFul);
  TEST_FAIL_IF(1023 != 0x3FFlu);
  TEST_FAIL_IF(1023 != 0x3FFuLL);
  TEST_FAIL_IF(1023 != 0x3FFllU);
  TEST(1);
}

/*
 * if x is 'unsigned', result is always positive.
 * if x is 'signed', result is always negative.
 */
#define NEG_I(x)    ((x) | (int)0x80000000)
#define NEG_LL(x)   ((x) | (long long)0x8000000000000000)

TEST_CASE(C99_6_4_4_1_integer_constants_2)
{
  TEST_FAIL_IF(sizeof(2147483647) != sizeof(int));
  TEST_FAIL_IF(NEG_I(2147483647) > 0);
  TEST_FAIL_IF(sizeof(2147483648) != sizeof(long long));
  TEST_FAIL_IF(NEG_LL(2147483648) > 0);
  TEST_FAIL_IF(sizeof(2147483647LL) != sizeof(long long));
  TEST_FAIL_IF(NEG_LL(2147483647LL) > 0);
  TEST_FAIL_IF(sizeof(2147483648U) != sizeof(unsigned int));
  TEST_FAIL_IF(NEG_I(2147483648U) < 0);
  TEST_FAIL_IF(sizeof(4294967295) != sizeof(long long));
  TEST_FAIL_IF(NEG_LL(4294967295) > 0);
  TEST_FAIL_IF(sizeof(4294967295U) != sizeof(unsigned int));
  TEST_FAIL_IF(NEG_I(4294967295U) < 0);
  TEST_FAIL_IF(sizeof(4294967296U) != sizeof(unsigned long long));
  TEST_FAIL_IF(NEG_LL(4294967296U) < 0);
  TEST(1);
}

TEST_CASE(C99_6_4_4_1_integer_constants_3)
{
  TEST_FAIL_IF(sizeof(0x7FFFFFFF) != sizeof(int));
  TEST_FAIL_IF(NEG_I( 0x7FFFFFFF) > 0);
  TEST_FAIL_IF(sizeof(0x7FFFFFFFU) != sizeof(unsigned));
  TEST_FAIL_IF(NEG_I( 0x7FFFFFFFU) < 0);
  TEST_FAIL_IF(sizeof(0x80000000) != sizeof(unsigned));
  TEST_FAIL_IF(NEG_I( 0x80000000) < 0);
  
  TEST_FAIL_IF(sizeof(0x7FFFFFFFFFFFFFFF) != sizeof(long long));
  TEST_FAIL_IF(NEG_LL(0x7FFFFFFFFFFFFFFF) > 0);
  TEST_FAIL_IF(sizeof(0x7FFFFFFFFFFFFFFFU) != sizeof(unsigned long long));
  TEST_FAIL_IF(NEG_LL(0x7FFFFFFFFFFFFFFFU) < 0);
  TEST_FAIL_IF(sizeof(0x8000000000000000) != sizeof(unsigned long long));
  TEST_FAIL_IF(NEG_LL(0x8000000000000000) < 0);

  TEST(1);
}
