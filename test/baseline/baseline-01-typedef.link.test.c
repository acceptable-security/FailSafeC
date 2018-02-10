#include <stdlib.h>

/* originally test5-typedef.c */

typedef double t;

int main(void) {
    t t = -3.0;
    printf("%f", fabs(t));
}
