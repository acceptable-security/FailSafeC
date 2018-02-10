/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include "common.h"

static int x = 1;
struct x {
  int x;
};

static int f(struct x *x)
{
  {
    int x = 10;
    return x;
  }
}

TEST_CASE(C99_6_2_1_scope)
{
  TEST_FAIL_IF(x != 1);
  {
    char x = 4;
    TEST_FAIL_IF(x != 4);
    {
      struct x x = { 5 };
      TEST_FAIL_IF(x.x != 5);
      {
        enum x { x = 6 };
        TEST_FAIL_IF(x != 6);
        {
          enum x { x = 7 };
          TEST_FAIL_IF(x != 7);
          {
            struct x { int x[2]; } x = { {8, 9} };
            TEST_FAIL_IF(x.x[0] != 8);
            {
              typedef short x;
              TEST_FAIL_IF((x)sizeof(x) != sizeof(short));
              {
                x x = 9;
                TEST_FAIL_IF(x != 9);
                TEST_FAIL_IF(f(0) != 10);
                TEST_FAIL_IF(x != 9);
              }
              TEST_FAIL_IF((x)sizeof(x) != sizeof(short));
            }
            TEST_FAIL_IF(x.x[0] != 8);
          }
          TEST_FAIL_IF(x != 7);
        }
        TEST_FAIL_IF(x != 6);
      }
      TEST_FAIL_IF(x.x != 5);
    }
    TEST_FAIL_IF(x != 4);
    goto x;
  }
x:
  TEST_FAIL_IF(x != 1);
  TEST(1);
}

