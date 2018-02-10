<%# /* -*- c -*- */
#include <glob.h>
#%>
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

#define GLOB_APPEND   <%=d GLOB_APPEND   %>
#define GLOB_DOOFFS   <%=d GLOB_DOOFFS   %>
#define GLOB_ERR      <%=d GLOB_ERR      %>
#define GLOB_MARK     <%=d GLOB_MARK     %>
#define GLOB_NOCHECK  <%=d GLOB_NOCHECK  %>
#define GLOB_NOESCAPE <%=d GLOB_NOESCAPE %>
#define GLOB_NOSORT   <%=d GLOB_NOSORT   %>

#define GLOB_ABORTED  <%=d GLOB_ABORTED  %>
#define GLOB_NOMATCH  <%=d GLOB_NOMATCH  %>
#define GLOB_NOSPACE  <%=d GLOB_NOSPACE  %>
#define GLOB_NOSYS    <%=d GLOB_NOSYS    %>

extern int glob(const char *, int, int (*)(const char *, int), glob_t *);
extern void globfree(glob_t *);

#endif
