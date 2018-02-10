/* */
#include <sys/mman.h>
#include <stdio.h>
#include <unistd.h>
#include <setjmp.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <signal.h>

sigjmp_buf jmp;
volatile int next_cont;
volatile int sig;

#define CHECK(e) do { if (!(e)) { fprintf(stderr, "%s:%d: test `%s' failed.\n", __FILE__, __LINE__, #e); return 1; }} while (0);

void signal_handler(int _sig)
{
    sig = _sig;
    siglongjmp(jmp, 1);
}

int main(int argc, char **argv)
{
    int pagesize = getpagesize();
    void *region, *region2;
    struct stat stat;

    int fd, length;

    if (argc < 2) {
	printf("usage: %s [test_number]\n", argv[0]);
	return 1;
    }

    if ((fd = open("mmap_test1.c", O_RDONLY)) < 0) {
	perror("cannot open mmap_test.c");
	return 1;
    }
    if (fstat(fd, &stat) < 0) {
	perror("cannot stat mmap_test.c");
	return 1;
    }
    length = stat.st_size;

    if (sigsetjmp(jmp, 1) == 0) {
	next_cont = atoi(argv[1]);
	sig = 0;
	printf("running test %d...", next_cont);
    } else {
	printf("(signal %d)...", sig);
	if (next_cont < 10000)
	    abort();
	printf("(%d)...", next_cont);
    }
    fflush(stdout);

    signal(SIGSEGV, signal_handler);
    signal(SIGBUS, signal_handler);

    switch(next_cont) {
    case 0:
	region = mmap(0, pagesize * 2, PROT_READ, MAP_SHARED, fd, 0);
	CHECK(region != (void *)MAP_FAILED);
	CHECK(((char *)region)[0] == '/');
	break;
    case 1:
	region = mmap(0, length, PROT_READ, MAP_SHARED, fd, 0);
	CHECK(region != (void *)MAP_FAILED);
	region2 = mmap(0, length, PROT_READ, MAP_SHARED, fd, 0);
	CHECK(region2 != (void *)MAP_FAILED);
	CHECK(0 == memcmp(region, region2, length));
	break;
    case 2:
	region = mmap(0, length, PROT_READ, MAP_SHARED, fd, 0);
	CHECK(region != (void *)MAP_FAILED);
	next_cont = 20001;
	((char *)region)[0] = '*';
	CHECK(0);
    case 20001:
	CHECK(sig != 0);
	break;
    case 3:
	region = mmap(0, pagesize * 2, PROT_READ, MAP_SHARED, fd, 0);
	CHECK(region != (void *)MAP_FAILED);
	region2 = mmap((void *)((char *)region + pagesize), pagesize, PROT_READ,
		       MAP_FIXED | MAP_SHARED, fd, 0);
	CHECK(region2 != (void *)MAP_FAILED);
	CHECK(0 == memcmp(region, region2, pagesize));
	break;
    case 4:
	region = mmap(0, length + pagesize, PROT_READ, MAP_SHARED, fd, 0);
	CHECK(region != (void *)MAP_FAILED);
	CHECK(((char *)region)[0] == '/');
	CHECK(0 == munmap(region, length));
	next_cont = 40001;
	CHECK(((char *)region)[0] == '/');
	CHECK(0);
    case 40001:
	CHECK(sig != 0);
	break;
    case 5:
	region = mmap(0, pagesize * 2, PROT_READ, MAP_SHARED, fd, 0);
	CHECK(region != (void *)MAP_FAILED);
	CHECK(((char *)region)[0] == '/');
	CHECK(0 == mprotect(region, length, PROT_NONE));
	next_cont = 50001;
	CHECK(((char *)region)[0] == '/');
	CHECK(0);
    case 50001:
	CHECK(sig != 0);
	break;
    default:
	abort();
    }
    printf("done.\n");
    return 0;
}
