#include <stdio.h>

void test(char *s, char *f) {
    static int n = 0;
    int r, i = 88888;
    char c = '_';

    n++;

    r = sscanf(s, f, &i, &c);
    printf ("%5d(%2d): %5d, '%c'\n", n, r, i, c);
}

int main(void) {
    test("0", "%i%c");
    test("0 x", "%i%c");
    test("+x", "%i%c");
    test("-x", "%i%c");
    test("+0x", "%i%c");
    test("+0y", "%i%c");
    test("0xy", "%i%c");
    test("0xax", "%i%c");
    test("09y", "%i%c");
    test("0xfgh", "%i%c");
    return 0;
}
