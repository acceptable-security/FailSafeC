#include <stdlib.h>
#include <stdio.h>
#include <string.h>

struct type_table { char *name; void *type; }
  type_table[] = {
    { "int", __typeof(int) },
    { "char", __typeof(char) },
    { "int *", __typeof(int *) },
    { "char *", __typeof(char *) },
    { "long long", __typeof(long long) },
    { NULL, NULL }
};

int main(int argc, char **argv) {
    int size = 10;
    int i;
    int *pi;
    char *pc;
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

    pi = (int *)malloc_typed(size * sizeof(int), tp->type);

    printf ("integer write\n");
    for(i = 0; i < size; i++) {
	pi[i] = i;
    }
    printf ("integer read\n");
    for(i = 0; i < size; i++) {
	printf ("%d ", pi[i]);
    }

    printf ("\nchar read\n");
    pc = (char *)pi;
    for(i = 0; i < size * sizeof(int); i++) {
      printf("%d ", pc[i]);
    }

    printf ("\nchar write\n");
    for(i = 0; i < size * sizeof(int); i++) {
      pc[i] = i;
    }

    printf ("integer read\n");
    for(i = 0; i < size; i++) {
	printf ("%#x ", pi[i]);
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
    
    printf ("integer read\n");
    for(i = 0; i < size; i++) {
	printf ("%#x ", pi[i]);
    }

    printf ("done\n");
    return 0;
}
