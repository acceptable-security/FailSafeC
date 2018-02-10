int main(int argc, char *argv[]) {
    goto L1;

 L2:
    printf("%d: %p", 2, argv[0]);
    goto L3;

 L1:
    printf("%d: %p", 3, argv[0]);
    goto L2;

 L3:
    return 0;
}
