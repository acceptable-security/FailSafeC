struct S { int x, y; } g = { 1, 2 };

int main(void)
{
    struct S x = g;

    return !(g.x == 1);
}
