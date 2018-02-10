#include <stdio.h>
#include <time.h>

int main(void) {
    time_t t = time(0);
    struct tm *l;

    puts(ctime(&t));

    l = localtime(&t);

    printf ("%d %d %d -- %d:%d:%d, %d/w %d/y, isdst %d\n",
	    l->tm_year, l->tm_mon + 1, l->tm_mday,
	    l->tm_hour, l->tm_min, l->tm_sec,
	    l->tm_wday, l->tm_yday, l->tm_isdst);

    puts(asctime(l));

    return 0;
}

