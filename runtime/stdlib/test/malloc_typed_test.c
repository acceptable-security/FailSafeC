#include <stdlib.h>
#include <stdio.h>

int main(int argc, char **argv) {
    int size = 10, i, *p;
    if (argc >= 2) {
	size = atoi(argv[1]);
    }

    p = (int *)malloc_typed(size * sizeof(int), __typeof(int));

    for(i = 0; i < size; i++) {
	p[i] = i;
    }

    free (p);

    if (argc >= 3 && argv[2][0] == 'd')
	free (p);
    
    if (argc >= 3 && argv[2][0] == 'a')
	p[0] = 0;
    
    return 0;
}
