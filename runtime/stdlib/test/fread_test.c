#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

int main(int argc, char *argv[]) {
    FILE *fp = fopen(argv[1], "r");
    FILE *wfp = argv[2] ? fopen(argv[2], "w") : NULL;
    int buf[256];
    int i = 0;
    
    if (!fp) {
	fprintf (stderr, "open failed: %d\n", errno);
	return 1;
    }
    while (i = fread(buf, 1, 255, fp)) {
	printf ("%d=", i);
	if (wfp && i != fwrite(buf, 1, i, wfp)) perror("fwrite");
	printf ("%d ", buf[0]);
    }
    
    fclose(fp);
    if (wfp) fclose(wfp);
    return 0;
}
