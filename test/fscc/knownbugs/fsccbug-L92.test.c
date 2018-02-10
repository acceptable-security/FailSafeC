#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "common.h"

struct X {
  char *p;
} x, y;

TEST_CASE(fsccbug_L92)
{
  char *p = malloc(sizeof(struct X));

  x.p = malloc(1);
  *x.p = 12;
  
  memcpy(p, &x, sizeof(struct X));
  memcpy(&y, p, sizeof(struct X));

  TEST(*y.p == 12);
}
