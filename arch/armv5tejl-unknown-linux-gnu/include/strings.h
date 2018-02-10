/* Generated file -- do not edit. */
/**
 * @file include/strings.h
 */
#ifndef __STRINGS_H
#define __STRINGS_H

#include <sys/__types.h>

#ifndef __SIZE_T
#define __SIZE_T
typedef __size_t size_t;
#endif

extern int bcmp(const void *, const void *, size_t);
extern void bcopy(const void *, void *, size_t);
extern void bzero(void *, size_t);

extern int ffs(int);

extern char *index(const char *, int);
extern char *rindex(const char *, int);

extern int strcasecmp(const char *, const char *);
extern int strncasecmp(const char *, const char *, size_t);

#endif
