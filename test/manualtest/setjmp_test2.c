#include <setjmp.h>
#include <stdio.h>

int main(void) {
    jmp_buf b;
    int r = 5;

    if ((r = setjmp(b)) != 5) {
	printf("%d\n", r);
	longjmp(b, r + 1);
    }

    return 0;
}
