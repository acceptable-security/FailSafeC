#include <memory.h>
#include <stdio.h>
#include <ctype.h>

int ibuf[32 / sizeof(int)];

void dump(void) {
    int i;
    char *p = (char *)ibuf;

    for(i = 0; i < 32; i++) {
	putchar(isascii(p[i]) ? p[i] : '.');
    }
    putchar('\n');
}

int main(void) {
    char *buf = (char *)ibuf;
    memmove(buf, "abcdefghijklmnopqrstuvwxyz789012", 32);
    printf("first status:                   ");
    dump();
    printf("aligned move: (4-20) -> 16--32: ");
    memmove(buf + 16, buf + 4, 16);
    dump();
    printf("unalgnd move: (4-15) -> 13--24: ");
    memmove(buf + 13, buf + 4, 11);
    dump();
    printf("unalgnd move: (4-9)  ->  0--5 : ");
    memmove(buf + 0, buf + 4, 5);
    dump();
    return 0;
}
