/**
 * @file include/string.h
 */
#ifndef __STRING_H
#define __STRING_H

#include <sys/__types.h>

#ifndef NULL
#define NULL 0
#endif

#ifndef __SIZE_T
#define __SIZE_T
typedef __size_t size_t;
#endif

extern void *memccpy(void *, const void *, int, size_t);
extern void *memchr(const void *, int, size_t);
extern int memcmp(const void *, const void *, size_t);
extern void *memcpy(void *, const void *, size_t);
extern void *memmove(void *, const void *, size_t);
extern void *memset(void *, int, size_t);

extern char *strcat(char *, const char *);
extern char *strchr(const char *, int);
extern int strcmp(const char *, const char *);
extern int strcoll(const char *, const char *);
extern char *strcpy(char *, const char *);
extern size_t strcspn(const char *, const char *);
extern char *strdup(const char *);

extern char *strerror(int);
extern int *strerror_r(int, char *, size_t);

extern size_t strlen(const char *);
extern char *strncat(char *, const char *, size_t);
extern int strncmp(const char *, const char *, size_t);
extern char *strncpy(char *, const char *, size_t);
extern char *strpbrk(const char *, const char *);
extern char *strrchr(const char *, int);
extern size_t strspn(const char *, const char *);
extern char *strstr(const char *, const char *);

extern char *strtok(char *, const char *);
extern char *strtok_r(char *, const char *, char **);
extern size_t strxfrm(char *, const char *, size_t);

/* non-standard */
#include <strings.h>

#endif
