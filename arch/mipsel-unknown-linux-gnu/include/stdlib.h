/* Generated file -- do not edit. */
/**
 * @file include/stdlib.h
 */
#ifndef __STDLIB_H
#define __STDLIB_H

#include <sys/__types.h>

#ifndef __SIZE_T
#define __SIZE_T
typedef __size_t size_t;
#endif

#ifndef __PTRDIFF_T
#define __PTRDIFF_T
typedef __ptrdiff_t ptrdiff_t;
#endif

#include <sys/types.h>

typedef struct __fsc_attribute__((named "stdlib_div_t")) _div_t {
  int quot;
  int rem;
} div_t;

typedef struct __fsc_attribute__((named "stdlib_ldiv_t")) _ldiv_t {
  long quot;
  long rem;
} ldiv_t;

typedef struct __fsc_attribute__((named "stdlib_lldiv_t")) _lldiv_t {
  long long quot;
  long long rem;
} lldiv_t;

extern long strtol(const char *, char **, int);
extern unsigned long strtoul(const char *, char **, int);

extern long long strtoll(const char *, char **, int);
extern unsigned long long strtoull(const char *, char **, int);

extern float strtof(const char *, char **);
extern double strtod(const char *, char **);
extern long double strtold(const char *, char **);


extern int atoi(const char *);
extern long atol(const char *);
extern long long atoll(const char *);
extern double atof(const char *);
extern void __fsc_attribute__((noreturn)) exit(int status);

extern void *malloc_typed(size_t size, void *type);
extern void *malloc(size_t size);
extern void *calloc(size_t, size_t);
extern void *realloc(void *, size_t size);
extern void free(void *);
extern int abs(int);
extern void __fsc_attribute__((noreturn)) abort(void);

extern char *getenv(const char *name);
extern int setenv(const char *, const char *, int);
extern int putenv(const char *);
extern int unsetenv(const char *);

extern void qsort(void *, size_t, size_t, 
		  int(*)(const void *, const void *));
extern void *bsearch(const void *, const void *, size_t, size_t,
                     int(*)(const void *, const void *));

extern int rand(void);
extern void srand(unsigned seed);

extern long random(void);
extern void srandom(unsigned seed);

extern void __fsc_attribute__((noreturn)) _Exit(int);
extern div_t div(int, int);
extern ldiv_t ldiv(long, long);
extern lldiv_t lldiv(long long, long long);
extern double drand48(void);
extern double erand48(unsigned short [3]);
extern char *fcvt(double, int, int *, int *);
extern char *ecvt(double, int, int *, int *);
extern char *gcvt(double, int, char *);
extern int grantpt(int);
extern long jrand48(unsigned short [3]);
extern long labs(long);
extern void lcong48(unsigned short [7]);
extern long lrand48(void);
extern long long llabs(long long);
extern int mkstemp(char *);
extern char *mkdtemp(char *);
extern char *mktemp(char *);
extern long mrand48(void);
extern long nrand48(unsigned short [3]);
extern int posix_openpt(int);
extern char *ptsname(int);
extern char *realpath(const char *, char *);
extern short *seed48(unsigned short [3]);
extern char *setstate(const char *);
extern void srand48(long);
extern int unlockpt(int);

extern int system(const char *);

extern long a64l(const char *);
extern char *l64a(long value);

#ifndef NULL
#define NULL 0
#endif

#define EXIT_SUCCESS 0
#define EXIT_FAILURE 1
#define RAND_MAX 2147483647

#endif
