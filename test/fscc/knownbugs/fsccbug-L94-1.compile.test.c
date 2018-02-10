/* acceptable K&R declarations */

void f1() {}

void f2(a) {}

void f2_2(a) int a; {}

void f4(a,b) int a; {}

void f42(a,b) int a, b; {}

void f5(a,b) int a; double b; {}

void (*f6(a,b))(int) int a; double b; { return (void *)(int)(a + b); }

/* void (*f7(int))(a,b) int a; double b; {} /* this should be error */

