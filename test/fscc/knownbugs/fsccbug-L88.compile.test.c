#include <stdio.h>

struct s { int x; };

int main(int argc, char **argv) {
    struct s a = {1}, b = {2};
    printf ("%d\n", (argc == 1 ? a : b).x);
    return 0;
}
