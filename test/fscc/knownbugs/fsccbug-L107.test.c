#include "common.h"
#include <stdio.h>

TEST_CASE(fsccbug_L107)
{
    char buf[80] = "abcde";
    int v = 0;
    sprintf(buf, "%.*s", 0, &v);
    TEST(buf[0] == '\0');
}
