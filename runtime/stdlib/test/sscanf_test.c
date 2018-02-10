#include <stdlib.h>
#include <stdio.h>

int main(int c, char **argv) {
    char buf[40];
    int v;
    char *p;
    for(;;) {
	int i;
#if 1
	p = gets(buf);
#else
	p = fgets(buf, 40, stdin);
#endif
	if (p == NULL) 
	    break;
	printf("input str = \"%s\"\n", buf);
	v = sscanf(buf, argv[1], &i);
	printf("retval = %d, i = %d\n", v, i);
    }
    return 0;
}
