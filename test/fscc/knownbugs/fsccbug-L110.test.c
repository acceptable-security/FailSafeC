#include "common.h"
#include <setjmp.h>

TEST_CASE(fsccbug_L110)
{
    int ret, ok = 0;
    jmp_buf buf1, buf2;

    ret = setjmp(buf1);
    if (ret == 0) {
	memcpy(buf2, buf1, sizeof(buf1));
	longjmp(buf2, 1);
    } else {
	ok = 1;
    }
    TEST(ok);
}
