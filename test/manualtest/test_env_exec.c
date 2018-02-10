#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>

#ifndef TEST
#define TEST (-1)
#endif

char *env[] = { "TEST1=test1", NULL };
char *args1[] = { "env", NULL };
extern char **environ;

int main(int argc, char **argv) {
    char **p;
    int i = 0;

    putenv("TEST2=test2");

    switch (argc < 2 ? -1 : atoi(argv[1])) {
    case 0:
	execv("/usr/bin/env", args1);
	break;
    case 1:
	execvp("env", args1);
	break;
    case 2:
	execve("/usr/bin/env", args1, env);
	break;
    case 3:
	execl("/usr/bin/env", "env", NULL);
	break;
    case 4:
	execle("/usr/bin/env", "env", NULL, env);
	break;
    default:
	for(p = environ; *p; p++) {
	    printf("%d: %s\n", i++, *p);
	}
	return 0;
    }
    perror("exec");
    return 0;
}
