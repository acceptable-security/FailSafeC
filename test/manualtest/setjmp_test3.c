#include <setjmp.h>
#include <stdio.h>
#include <stdlib.h>

jmp_buf b, bb;

int test(int i) {
    int x;
    setjmp(bb);
    if (i > 100)
	longjmp(b, i);
    else
	test(i * 2 + (int)&x * 0);
}

int main(int c, char **v) {
    int r = 5;

    if (r = setjmp(b)) {
	printf("returned from setjmp: %d\n", r);
	if (r == 160) longjmp(bb, 5);
	if (r == 180) longjmp(b, 1);
	if (r == 200) {
	  printf("b=%p, b[0]=%p\n", b, b[0]);
	  *(int *)&b = 0;
        }
	return 0;
    }
    test(atoi(v[1]));

    return 0;
}

/* 5: 160 + trap: jumping to dead frame.
   25: 200 + trap: abstract data access.
   45: 180 + 1 */
