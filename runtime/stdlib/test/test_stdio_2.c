#include <stdio.h>

char s0[9]="11111111";
int s1[3]={0x32323232, 0x32323232, 0x0};
/*char s2[8]="33333333";*/
int s2[2]={0x33333333, 0x33333333};

int main(int argc, char *argv[]) {
    puts(s0);
    puts((char *)s1);
    puts((char *)s2); /* should raise access error */
}
