#include "common.h"
#include <limits.h>

typedef struct {
  int a:(sizeof(int) * CHAR_BIT);
  int b:(sizeof(int) * CHAR_BIT);
} T;

TEST_CASE(fsccbug_L118)
{
  int buf[2];
  T *p = (T*)buf;

  buf[0] = buf[1] = 0;

  p->a = ~0;
  p->b = ~0;

  TEST(buf[1] != 0);
}
