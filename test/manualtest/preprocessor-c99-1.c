/* Preprocessor tests */

#define ss(a) #a
#define s(a) ss(a)

/* In C90, parsed as three tokens 0xa.bcdep + abcdefg, 
 *         generates 0xa.bcdep+XYZZY */
/* In C99, parsed as one invalid hexadecimal float-like token 0x.abcdep+abcdefg,
 *         generates 0xa.bcdep+abcdefg */

#define abcdefg XYZZY

char *p = s(0xa.bcdep+abcdefg)

#undef abcdefg

