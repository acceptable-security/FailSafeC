struct x {
  int y;
} k;

void foo(struct x z)
{
  z.y;
  z = k;
  z.y;
}
