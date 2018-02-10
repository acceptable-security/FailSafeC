#include <stdio.h>

struct S {
    int f;
};

struct S x1, x2;
int y;
int z = 5;

int main(void)
{
    static int s = 6;
    static int t;
    int u;
    int v = 5;

    sscanf("0 0", "%d %d", &u, &v);

    printf ("x1=%d x2=%d y=%d z=%d s=%d t=%d\n", x1.f, x2.f, y, z, s, t);
    return 0;
}
