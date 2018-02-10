void putc(char);

int main(void) {
  void (*f)(char);
  int x;
  f = putc;
  putc("Hello"[x]);
  f("Hello"[x]);
}

