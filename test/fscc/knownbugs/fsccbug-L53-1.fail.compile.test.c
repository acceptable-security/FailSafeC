#ifdef __GNUC__
#define __fsc_attribute__(x)
#endif

struct foo {int n;} *func(void);

struct __fsc_attribute__((named "stdlib_foo")) foo {
  int n;
};

int main(int argc, char **argv){
  return 0;
}
