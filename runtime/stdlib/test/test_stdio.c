#include <stdio.h>

int main(int argc, char **argv) {
    puts("puts: s");
    fputs("fputs to stderr\n", stderr);
    fputs("fputs to stdout\n", stdout);
    fputc('*', stderr);
    fputs("fputc to stderr\n", stderr);
    putchar('*');
    puts("fputchar");
    return 0;
}
