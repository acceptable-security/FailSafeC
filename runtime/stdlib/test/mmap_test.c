#include <unistd.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>

extern void *mmap(void *, int, int, int, int, int);
extern int munmap(void *, int);
extern void GC_gcollect(void);

void signal_handler(int sig, siginfo_t *sinfo, void *addr) {
    fprintf(stderr, "access fault %d at %lx\n", sig, (long)addr);
    abort();
}

int main(int argc, char **argv) {
    struct stat t;
    int fd = open("mmap_test.c", O_RDONLY);
    int i, len, o1, o2;
    char *p;

    fstat(fd, &t);

    o1 = (argc > 1 ? atoi(argv[1]) : 0);
    o2 = (argc > 2 ? atoi(argv[2]) : o1);
    if (argc > 3) {
	struct sigaction sa;
	sa.sa_sigaction = signal_handler;
	sa.sa_flags = SA_ONESHOT | SA_SIGINFO;
	sigaction(SIGBUS, &sa, 0);
	sigaction(SIGSEGV, &sa, 0);
	fprintf(stderr, "setting signals\n");
    }

    p = mmap(0, t.st_size + o1, 1 /* PROT_READ */, 1 /* MAP_SHARED */, fd, 0);

    close(fd);

    fprintf(stderr, "address: %p\n", p, &p);

    for(i = 0; i < t.st_size + o2; i++) {
	fprintf(stdout, "%c", p[i]);
	fflush(stdout);
    }

    munmap(p, t.st_size + o1);

    p = 0;

    GC_gcollect();

    return 0;
}
