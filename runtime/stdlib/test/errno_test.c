#include <errno.h>
#include <stdio.h>

int main(void) {
    int i = 0;
    for (i = 1; i < 10; i++) {
	i = errno;
	printf("%d\n", errno);
	perror("perror(1)");
    }
    printf("%d\n", errno);
    perror("perror(2)");
    return 0;
}
