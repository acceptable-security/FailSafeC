#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*char buf[256];*/
int buf[256 / sizeof(int)];

int main(int argc, char **argv) {
    char *p = (char *)buf;
    int x = 1;
    int i = atoi(argv[1]);
    
    if (i) {
	x++;
	p += 256 - i;
    }
    printf("%p\n", p);

    for (; argv[x]; x++) {
	sprintf(p, "%s%c", argv[x], '<');
	printf("%d:", strlen(p));
	puts(p);
    }
    return 0;
}
