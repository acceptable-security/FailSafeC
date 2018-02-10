<%# /* -*- c -*- */
#define _XOPEN_SOURCE 500
#include <fnmatch.h>
#%>
/**
 * @file fnmatch.h
 */

#ifndef __FNMATCH_H
#define __FNMATCH_H

#define FNM_NOMATCH  <%=d FNM_NOMATCH %>
#define FNM_PATHNAME <%=d FNM_PATHNAME %>
#define FNM_PERIOD   <%=d FNM_PERIOD %>
#define FNM_NOESCAPE <%=d FNM_NOESCAPE %>
#define FNM_NOSYS    <%=d FNM_NOSYS %>

extern int fnmatch(const char *, const char *, int);

#endif
