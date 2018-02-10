#include <stdio.h>

int main(int c, char **argv) {
    for(;;) {
	int i, j;
	int v = scanf(argv[1], &i, &j);
	printf("retval = %d, i = %d, j = %d\n", v, i, j);
    }
}
