#ifdef __GNUC__
#define __fsc_attribute__(x)
#endif

/* this pattern is not accepted by current specification. */

struct foo {
  int n;
} *func(void);

struct __fsc_attribute__((named "test_foo")) foo x;

int main(int argc, char **argv){
  return 0;
}
