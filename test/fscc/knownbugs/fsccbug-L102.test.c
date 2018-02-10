#include "common.h"
#include <stdio.h>

TEST_CASE(fsccbug_L102_1)
{
  char a[6] = { 0x00, 0x11, 0x22, 0x33, 0x44, 0x55 };

  int x = *(int*)&a[2];

  *(int*)&a[2] = x;
  
  TEST(x == 0x55443322 || x == 0x22334455);
}
