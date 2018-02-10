extern char **environ;

#include <stdio.h>

int main(void) {
    char **p;
    int i = 0;

    for(p = environ; *p; p++) {
	printf("%d: %s\n", i++, *p);
    }
    return 0;
}
