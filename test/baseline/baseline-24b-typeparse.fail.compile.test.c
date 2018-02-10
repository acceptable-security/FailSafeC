typedef int I;
typedef I *PI;
typedef PI FPI(void);

FPI afpi[5]; /* declaring an array of function */

extern FPI g;
FPI f = g; /* initializing function */
