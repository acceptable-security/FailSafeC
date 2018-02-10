struct st {
  int x;
};

struct st foo(void) {
  struct st s = { 0 };
  return s;
}

int main(int argc, char *argv[]) {
  struct st (*funcp)(void) = foo;
  funcp();
  return 0;
}
