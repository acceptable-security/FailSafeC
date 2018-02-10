/* override variable by typedef */

/*extern int i = 1;
typedef double i;
typedef int i;
static int i = 2;
*/


extern i1;
extern i2;
extern i3;
extern i4;
extern i5;

int i1 = 1;
static i2 = 2;
extern int i3 = 3;
int i4;
static int i5;

int i1;
/* int i2; */
int i3;
int i4;
/* int i5; */

extern i1;
extern i2;
extern i3;
extern i4;
extern i5;

int a[];
extern a[3];
