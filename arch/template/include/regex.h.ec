<%#/* -*- C -*- */
#include <regex.h>
#%>
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

#define REG_EXTENDED <%=d REG_EXTENDED %>
#define REG_ICASE    <%=d REG_ICASE %>
#define REG_NOSUB    <%=d REG_NOSUB %>
#define REG_NEWLINE  <%=d REG_NEWLINE %>
#define REG_NOSUB    <%=d REG_NOSUB %>
#define REG_NOTBOL   <%=d REG_NOTBOL %>
#define REG_NOTEOL   <%=d REG_NOTEOL %>
#define REG_NOMATCH  <%=d REG_NOMATCH %>
#define REG_BADPAT   <%=d REG_BADPAT %>
#define REG_ECOLLATE <%=d REG_ECOLLATE %>
#define REG_ECTYPE   <%=d REG_ECTYPE %>
#define REG_EESCAPE  <%=d REG_EESCAPE %>
#define REG_ESUBREG  <%=d REG_ESUBREG %>
#define REG_EBRACK   <%=d REG_EBRACK %>
#define REG_EPAREN   <%=d REG_EPAREN %>
#define REG_EBRACE   <%=d REG_EBRACE %>
#define REG_BADBR    <%=d REG_BADBR %>
#define REG_ERANGE   <%=d REG_ERANGE %>
#define REG_ESPACE   <%=d REG_ESPACE %>
#define REG_BADRPT   <%=d REG_BADRPT %>

extern int regcomp(regex_t *preg, const char *pattern, int cflags);
extern int regexec(const regex_t *preg, const char *string, size_t nmatch, regmatch_t pmatch[], int eflags);
extern size_t regerror(int errcode, const regex_t *preg, char *errbuf, size_t errbuf_size);
extern void regfree(regex_t *preg);

#endif
