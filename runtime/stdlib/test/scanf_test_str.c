#include <stdio.h>

int main(int c, char **argv) {
    for(;;) {
	int i;
	char s[80];
	int v = scanf(argv[1], &s);
	printf("retval = %d, i = \"%s\"\n", v, s);
    }
}
