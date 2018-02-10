#include <stdarg.h>
#include <stdio.h>

void test(int a, ...) {
    va_list ap;
    int k;

    va_start(ap, a);
    for(k = 0;; k++) {
	int i = va_arg(ap, int);
	if (i == 0)
	    break;
	printf ("%d: %d\n", k, i);
    }
    printf ("done\n");
    va_end(ap);
    printf ("end called\n");
}

int main(int argc, char **argv) {
    test(1,2,3,4,5,6,7,8,9,10,0);
}
