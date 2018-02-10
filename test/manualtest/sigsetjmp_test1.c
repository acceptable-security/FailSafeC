#include <setjmp.h>
#include <stdio.h>

int main(void) {
    sigjmp_buf b;
    int r;

    r = sigsetjmp(b,1);
    return 0;
}
