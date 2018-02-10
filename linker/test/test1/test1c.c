extern struct S d, *testB(void);

int testC(struct S *p) {
    return &d == testB();
}

