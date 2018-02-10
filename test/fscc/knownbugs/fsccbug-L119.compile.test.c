

int x0[2];
int *y0 = &(x0[1]);

int x1[2][2];
int *y1 = &x1[1][1];

int x2[2][2][2];
int *y2 = &x2[1][1][1];

void test(int) {
    int *y[3] = { &(x0[1]), &x1[1][1], &x2[1][1][1] };

    test(x0[1]);
    test(x1[1][1]);
    test(x2[1][1][1]);
}
