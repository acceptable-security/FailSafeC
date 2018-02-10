#include <stdio.h>

struct FILE {
    short s[2];
} s;

int main(int argc, char **argv) {
    s.s[0] = 1;
    s.s[1] = 2;
    fputs("die", &s);
}
