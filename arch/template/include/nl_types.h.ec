<%# /* -*- c -*- */
#include <nl_types.h>
#%>
/**
 * @file include/nl_types.h
 */
#ifndef _NL_TYPES_H
#define _NL_TYPES_H

#include <_private_storage.h>

typedef struct _private_storage *nl_catd;
typedef int nl_item;

#define NL_SETD <%=d NL_SETD %>
#define NL_CAT_LOCALE <%=d NL_CAT_LOCALE %>

int catclose(nl_catd);
char *catgets(nl_catd, int, int, const char *);
nl_catd catopen(const char *, int);

#endif
