#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv) {
    int size = 10, res = 0, i, *p;
    if (argc >= 2) {
	size = atoi(argv[1]);
    }
    if (argc >= 3) {
	res = atoi(argv[2]);
    }

    p = (int *)malloc(size * sizeof(int) + res);

    for(i = 0; i < size; i++) {
	p[i] = i;
    }

    free (p);

    if (argc >= 4 && argv[3][0] == 'd')
	free (p);
    
    if (argc >= 4 && argv[3][0] == 'a')
	p[0] = 0;
    
    return 0;
}
