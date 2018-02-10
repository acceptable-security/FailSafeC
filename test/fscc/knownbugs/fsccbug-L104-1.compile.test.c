void foo_1() {} /* type void(void) */

void (*bar_1)(void) = foo_1;

extern void foo_2(); /* type void(...) */

void (*bar_2)() = foo_2;
