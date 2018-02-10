#include <stdio.h>
int main(void) {
    char b1[80], b2[80], c1, c2;
    int r, i, j; short si;

    r = sscanf("abc def ghi", "%s%*s%n%s", &b1, &i, &b2);
    printf ("1(%d): \"%s\", (%d), \"%s\"\n", r, b1, i, b2);

    r = sscanf("abc def ghi", "%s%*s%hn%s", &b1, &si, &b2);
    printf ("2(%d): \"%s\", (%d), \"%s\"\n", r, b1, (int)si, b2);

    r = sscanf("123 ghi", "%d%n%s", &j, &i, &b2);
    printf ("3(%d): %d, (%d)%n, \"%s\"\n", r, j, i, &i, b2);
    printf ("3': (%d)\n", i);

    r = sscanf("123456 ghi", "%d%*n%s", &j, &b2);
    printf ("4(%d): %d, (%d), \"%s\"\n", r, j, i, b2);

    return 0;
}
