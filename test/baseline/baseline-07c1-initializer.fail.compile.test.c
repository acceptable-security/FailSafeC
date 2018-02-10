#include "common.h"
#include <stdio.h>

/* initializers: failure cases */

static int x = 0;
static int *y = ((int *)4) + ((int)&x);
