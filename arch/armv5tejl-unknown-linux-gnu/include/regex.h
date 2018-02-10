/* Generated file -- do not edit. */
/**
 * @file include/regex.h
 */
#ifndef __REGEX_H
#define __REGEX_H
#include <_private_storage.h>

typedef int regoff_t;

typedef struct __fsc_attribute__((named "stdlib_regex_t")) _regex_t {
  size_t re_nsub;
  struct _private_storage *__data;
} regex_t;

typedef struct __fsc_attribute__((named "stdlib_regmatch_t")) _regmatch_t {
  regoff_t rm_so;
  regoff_t rm_eo;
} regmatch_t;

#define REG_EXTENDED 1
#define REG_ICASE    2
#define REG_NOSUB    8
#define REG_NEWLINE  4
#define REG_NOSUB    8
#define REG_NOTBOL   1
#define REG_NOTEOL   2
#define REG_NOMATCH  1
#define REG_BADPAT   2
#define REG_ECOLLATE 3
#define REG_ECTYPE   4
#define REG_EESCAPE  5
#define REG_ESUBREG  6
#define REG_EBRACK   7
#define REG_EPAREN   8
#define REG_EBRACE   9
#define REG_BADBR    10
#define REG_ERANGE   11
#define REG_ESPACE   12
#define REG_BADRPT   13

extern int regcomp(regex_t *preg, const char *pattern, int cflags);
extern int regexec(const regex_t *preg, const char *string, size_t nmatch, regmatch_t pmatch[], int eflags);
extern size_t regerror(int errcode, const regex_t *preg, char *errbuf, size_t errbuf_size);
extern void regfree(regex_t *preg);

#endif
