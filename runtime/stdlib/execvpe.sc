/* 
   Part of Fail-Safe C Compiler. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2008 AIST.

   This file is written by Yutaka Oiwa in 2008. */

#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <limits.h>
#include <assert.h>

/* __execvpe: the missing function of the exec family. */
/*   roughly reimplements klibc's algorithm, with required changes */

int __execvpe(const char *file, char * const *argv, char * const *envp) {
    char *path;
    char buf[PATH_MAX + 1];
    char *p, *current_ptr, *next_ptr;
    int eacces_returned = 0;
    int file_len = 0;

    if (*file == '\0' || strchr(file, '/')) {
	return execve(file, argv, envp);
    }
    file_len = strlen(file);

    path = getenv("PATH");
    if (!path)
	path = ":/bin:/usr/bin";

    current_ptr = path;

    do {
	/* fprintf(stderr, "current ptr: %s\n", current_ptr); */
	p = strchr(current_ptr, ':');

	if (p == NULL) {
	    p = current_ptr + strlen(current_ptr);
	    next_ptr = NULL;
	} else {
	    next_ptr = p + 1;
	}

	if (p == current_ptr) {
	    /* fprintf(stderr, "executing (raw) %s\n", file); */
	    execve(file, argv, envp);
	    /* fprintf(stderr, "executing (raw) %s ... %s\n", file, strerror(errno)); */
	} else {
	    int len = (p - current_ptr) + 1 + file_len;
	    if (len > PATH_MAX) {
		/* fprintf(stderr, "path component will be too long (%d)\n", len); */
		errno = ENAMETOOLONG;
	    } else {
		memcpy(buf, current_ptr, p - current_ptr);
		buf[p - current_ptr] = '/';
		strcpy(buf + (p - current_ptr) + 1, file);
		assert(len == strlen(buf));
		/* fprintf(stderr, "executing %s\n", buf); */
		execve(buf, argv, envp);
		/* fprintf(stderr, "executing %s ... %s\n", buf, strerror(errno)); */
	    }
	}
	
	switch(errno) {
	default:
	    goto err;

	case EACCES:
	    /* follow Linux glibc semantics */
	    eacces_returned = 1;
	    break;

	case EISDIR:
	case ENOENT:
	case ENOEXEC:
	case ENAMETOOLONG:
	    break;
	}

	current_ptr = next_ptr;
    } while (current_ptr);
 err:
    if (eacces_returned)
	errno = EACCES;
    return -1;
}

/*
extern char **environ;

int main(int argc, char **argv) {
    setenv("PATH", argv[1], 1);
    __execvpe(argv[2], &argv[2], environ);
    perror("execvpe");
    return 1;
}
*/
