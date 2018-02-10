#include "common.h"
#include <stdio.h>

/* initializers: failure cases */

static int x = 0;
static int y = (int)&x + (int)&x;
