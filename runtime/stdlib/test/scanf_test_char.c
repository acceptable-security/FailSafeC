#include <stdio.h>
int main(void) {
    char b1[80], b2[80], c1, c2;
    int r;

    r = sscanf("abc def ghi", "%s%c%s", &b1, &c1, &b2);
    printf ("1(%d): \"%s\", '%c', \"%s\"\n", r, b1, c1, b2);

    r = sscanf("abcdefghi", "%[a-c]%c%[^h]%c", &b1, &c1, &b2, &c2);
    printf ("2(%d): \"%s\", '%c', \"%s\", '%c'\n", r, b1, c1, b2, c2);

    r = sscanf("abcdefghi", "%[a-c]%c%[^e]%c", &b1, &c1, &b2, &c2);
    printf ("3(%d): \"%s\", '%c', \"%s\", '%c'\n", r, b1, c1, b2, c2);

    r = sscanf("abcdefghi", "%[b-c]%c%[^e]%c", &b1, &c1, &b2, &c2);
    printf ("4(%d): \"%s\", '%c', \"%s\", '%c'\n", r, b1, c1, b2, c2);

    r = sscanf("", "%[b-c]%c%[^e]%c", &b1, &c1, &b2, &c2);
    printf ("5(%d): \"%s\", '%c', \"%s\", '%c'\n", r, b1, c1, b2, c2);

    r = sscanf("\200\201\202\203\204\205\206\207", "%[\200-\202]%c%[^\207]%c", &b1, &c1, &b2, &c2);
    printf ("6(%d): \"%s\", '%d', \"%s\", '%d'\n", r, b1, c1, b2, c2);

    return 0;
}
