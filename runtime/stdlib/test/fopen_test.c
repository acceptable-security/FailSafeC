#include <stdio.h>
#include <errno.h>

int main(int argc, char *argv[]) {
    FILE *fp = fopen(argv[1], "r");
    char buf[256];
    int i = 0;
    
    if (!fp) {
	fprintf (stderr, "open failed: %d\n", errno);
	return 1;
    }
    while (fgets(buf, 255, fp)) {
	printf ("%d: %s", i++, buf);
    }

    fclose(fp);
    return 0;
}

    
