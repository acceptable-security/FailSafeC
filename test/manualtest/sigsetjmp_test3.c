#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>

sigjmp_buf b, bb;

int test(int i) {
    sigsetjmp(bb, 1);
    if (i > 100)
	siglongjmp(b, i);
    else
	test(i * 2);
}

int main(int c, char **v) {
    int r = 5;

    if (r = sigsetjmp(b, 1)) {
	printf("returned from setjmp: %d\n", r);
	if (r == 160) siglongjmp(bb, 5);
	if (r == 180) siglongjmp(b, 1);
	if (r == 200) *(int *)b = 0;
	return 0;
    }
    test(atoi(v[1]));

    return 0;
}

/* 5: 160 + trap: jumping to dead frame.
   25: 200 + trap: abstract data access.
   45: 180 + 1 */
