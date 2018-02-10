#include <setjmp.h>
#include <stdio.h>

int main(void) {
    jmp_buf b;
    int r;

    r = setjmp(b);
    return 0;
}
