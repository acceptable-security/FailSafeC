#include <setjmp.h>
#include <stdio.h>

int main(void) {
    sigjmp_buf b;
    int r = 5;

    if ((r = sigsetjmp(b, 1)) != 5) {
	printf("%d\n", r);
	siglongjmp(b, r + 1);
    }

    return 0;
}
