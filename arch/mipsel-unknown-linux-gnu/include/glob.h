/* Generated file -- do not edit. */
/**
 * @file include/glob.h
 */
#ifndef __GLOB_H
#define __GLOB_H

#include <stddef.h>

typedef struct __fsc_attribute__((named "stdlib_glob_t")) __glob_t {
  size_t gl_pathc;
  char **gl_pathv;
  size_t gl_offs;
} glob_t;

#define GLOB_APPEND   32
#define GLOB_DOOFFS   8
#define GLOB_ERR      1
#define GLOB_MARK     2
#define GLOB_NOCHECK  16
#define GLOB_NOESCAPE 64
#define GLOB_NOSORT   4

#define GLOB_ABORTED  2
#define GLOB_NOMATCH  3
#define GLOB_NOSPACE  1
#define GLOB_NOSYS    4

extern int glob(const char *, int, int (*)(const char *, int), glob_t *);
extern void globfree(glob_t *);

#endif
