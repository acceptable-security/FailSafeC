#include <stdio.h>

int main(int argc, char **argv) {
    char buf[40];
    char buf2[40];
    int v, di;
    double d;
    for(;;) {
	if (gets(buf) == 0) break;
	v = sscanf(buf, "%lf%s", &d, buf2);
	di = d;
	printf("retval = %d, i = \"%20lg\" (%d), rest=\"%s\"\n", v, d, di, buf2);
    }
}
