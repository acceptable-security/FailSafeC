/* test7-funcprototype.c */

void f1();
void f2(void);
void f3(char *argv[]);
void f4(void signal(int));
void f5(void (*)(int));
void f6(void *(int));

void test(void) {
  f1(f1,f2,f3,f4,f5,f6);
}
