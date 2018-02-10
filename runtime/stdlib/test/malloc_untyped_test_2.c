#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct type_table { char *name; int kind; }
  type_table[] = {
    { "int", 0 },
    { "char", 1 },
    { "int *", 2 },
    { "char *", 3 },
    { NULL, NULL }
};

int main(int argc, char **argv) {
    int size = 10;
    int i;
    int *pi;
    void **ppv, **qpv;
    
    struct type_table *tp;
    
    for (tp = type_table; tp->name; tp++) {
	if (strcmp(argv[1], tp->name) == 0)
	    break;
    }

    if (tp->name == NULL) {
	fprintf(stderr, "type %s not found.\n", argv[1]);
	exit (1);
    }

    if (argc >= 3) {
	size = atoi(argv[2]);
    }

    pi = (int *)malloc(size * sizeof(int));

    switch (tp->kind) {
    case 0:
	*(int *)pi = 1;
	break;
    case 1:
	*(char *)pi = 1;
	break;
    case 2:
	*(int **)pi = &i;
	break;
    case 3:
	*(char **)pi = (char *)&i;
	break;
    }

    printf ("integer test\n");
    for(i = 0; i < size; i++) {
	pi[i] = i;
    }
    for(i = 0; i < size; i++) {
	printf ("%d ", pi[i]);
    }
    printf ("done\n");

    printf ("pointer test: write\n");
    ppv = qpv = (void **)pi;
    
    for(i = 0; i < size - 1; i++) {
	ppv[i] = (void *)&ppv[i + 1];
    }
    ppv[size - 1] = NULL;

    printf ("pointer test read\n");

    for(qpv = ppv; *qpv; qpv = (void **)*qpv) {
	printf ("%p\n", *qpv);
    }
    printf ("done\n");
    
    return 0;
}
