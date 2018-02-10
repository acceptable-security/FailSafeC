<%#shared /* -*- c -*- */
#include <sysexits.h>
#%>
/**
 * @file sysexits.h
 * TODO: should check BSD_SOURCE like definition?
 */
#ifndef __SYSEXITS_H
#define __SYSEXITS_H

#define EX_OK <%=d EX_OK %>

#define EX__BASE <%=d EX__BASE %>

#define EX_USAGE       <%=d EX_USAGE       %>
#define EX_DATAERR     <%=d EX_DATAERR     %>
#define EX_NOINPUT     <%=d EX_NOINPUT     %>
#define EX_NOUSER      <%=d EX_NOUSER      %>
#define EX_NOHOST      <%=d EX_NOHOST      %>
#define EX_UNAVAILABLE <%=d EX_UNAVAILABLE %>
#define EX_SOFTWARE    <%=d EX_SOFTWARE    %>
#define EX_OSERR       <%=d EX_OSERR       %>
#define EX_OSFILE      <%=d EX_OSFILE      %>
#define EX_CANTCREAT   <%=d EX_CANTCREAT   %>
#define EX_IOERR       <%=d EX_IOERR       %>
#define EX_TEMPFAIL    <%=d EX_TEMPFAIL    %>
#define EX_PROTOCOL    <%=d EX_PROTOCOL    %>
#define EX_NOPERM      <%=d EX_NOPERM      %>
#define EX_CONFIG      <%=d EX_CONFIG      %>

#endif /* __SYSEXITS_H */
