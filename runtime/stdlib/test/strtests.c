#include <string.h>
#include <stdio.h>

int main(int argc, char **argv) {
    char buf[80];
    char *tstr;

    tstr = (argc < 2 ? "test string" : argv[1]);

    printf ("tstr @ %p = \"%s\"\n", tstr, tstr);

    printf ("strlen(tstr) = %d\n", strlen(tstr));

    strcpy(buf, tstr);

    printf ("strcpy(buf, tstr) = \"%s\"\n", buf);

    strcat(buf, tstr);

    printf ("strcat(buf, tstr) = \"%s\"\n", buf);
    
    printf ("strcmp(tstr, \"abcde\") = %d\n", strcmp(tstr, "abcde"));

    printf ("strncmp(tstr, \"abcde\", 3) = %d\n", strncmp(tstr, "abcde", 3));

    printf ("strchr(tstr, 's') = %p\n", strchr(tstr, 's'));

    printf ("strrchr(tstr, 's') = %p\n", strrchr(tstr, 's'));

    printf ("strstr(tstr, \"str\") = %p\n", strstr(tstr, "str"));

    return 0;
}
