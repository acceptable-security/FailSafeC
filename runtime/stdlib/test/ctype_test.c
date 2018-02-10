#include <stdio.h>
#include <ctype.h>

int main(void) {
    char c, d, l, u;

    for (c = 1; c != 0; c++) {
	if (isprint(c))
	    d = c;
	else
	    d = '.';
	l = tolower(c);
	if (!isprint(l)) l = '.';
	u = toupper(c);
	if (!isprint(u)) u = '.';

	printf ("%4d (%c): L%c U%c: %s %s %s %s %s %s %s %s %s %s\n",
		c, d, l, u, 
		isalnum(c) ? "A-N" : "   ",
		isalpha(c) ? "ALP" : "   ",
		iscntrl(c) ? "CTL" : "   ",
		isdigit(c) ? "DIG" : "   ",
		islower(c) ? "LWR" : "   ",
		isgraph(c) ? "GRP" : "   ",
		isprint(c) ? "PRT" : "   ",
		ispunct(c) ? "PUN" : "   ",
		isspace(c) ? "SPC" : "   ",
		isupper(c) ? "UPR" : "   ",
		isxdigit(c)? "XDG" : "   ");
    }
    return 0;
}
