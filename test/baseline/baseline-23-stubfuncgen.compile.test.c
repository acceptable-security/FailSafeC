void test(int x, char c, double d, char *p){}
void testv(int x, char c, double d, char *p, ...){}
int testi(void){return 0;}
char testc(void){return 0;}
double testd(void){return 0;}
char *testp(void){return 0;}
void test_call(void) {
  void (*f)(int, char, double, void *p, ...) = (void (*)(int, char, double, void *, ...))testv;
  f(1,2,3.0,0,5);
}

