<%#shared /* -*- c -*- */
#include <syslog.h>
#%>
/**
 * @file syslog.h
 */
#ifndef __SYSLOG_H
#define __SYSLOG_H

#define LOG_PID    <%=d LOG_PID    %>
#define LOG_CONS   <%=d LOG_CONS   %>
#define LOG_NDELAY <%=d LOG_NDELAY %>
#define LOG_ODELAY <%=d LOG_ODELAY %>
#define LOG_NOWAIT <%=d LOG_NOWAIT %>

#define LOG_KERN   <%=d LOG_KERN   %>
#define LOG_USER   <%=d LOG_USER   %>
#define LOG_MAIL   <%=d LOG_MAIL   %>
#define LOG_NEWS   <%=d LOG_NEWS   %>
#define LOG_UUCP   <%=d LOG_UUCP   %>
#define LOG_DAEMON <%=d LOG_DAEMON %>
#define LOG_AUTH   <%=d LOG_AUTH   %>
#define LOG_CRON   <%=d LOG_CRON   %>
#define LOG_LPR    <%=d LOG_LPR    %>
#define LOG_LOCAL0 <%=d LOG_LOCAL0 %>
#define LOG_LOCAL1 <%=d LOG_LOCAL1 %>
#define LOG_LOCAL2 <%=d LOG_LOCAL2 %>
#define LOG_LOCAL3 <%=d LOG_LOCAL3 %>
#define LOG_LOCAL4 <%=d LOG_LOCAL4 %>
#define LOG_LOCAL5 <%=d LOG_LOCAL5 %>
#define LOG_LOCAL6 <%=d LOG_LOCAL6 %>
#define LOG_LOCAL7 <%=d LOG_LOCAL7 %>

#define LOG_EMERG   <%=d LOG_EMERG   %>
#define LOG_ALERT   <%=d LOG_ALERT   %>
#define LOG_CRIT    <%=d LOG_CRIT    %>
#define LOG_ERR     <%=d LOG_ERR     %>
#define LOG_WARNING <%=d LOG_WARNING %>
#define LOG_NOTICE  <%=d LOG_NOTICE  %>
#define LOG_INFO    <%=d LOG_INFO    %>
#define LOG_DEBUG   <%=d LOG_DEBUG   %>

<%(void)[[?%>
/* non-standard */
#define LOG_SYSLOG  <%=d LOG_SYSLOG  %>
<%]];%>

extern void closelog(void);
extern void openlog(const char *, int, int);
extern int setlogmask(int);
extern void syslog(int, const char *, ...);

#endif
