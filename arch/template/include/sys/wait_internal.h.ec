<%#shared /* -*- c -*- */
#include <sys/wait.h>
#%>
/**
 * @file include/sys/wait_internal.h
 */
#define WNOHANG		<%=d WNOHANG    %>
#define WUNTRACED	<%=d WUNTRACED  %>
<%(void)[[?%>#define WEXITED     <%=d WEXITED    %><%]];%>
<%(void)[[?%>#define WSTOPPED    <%=d WSTOPPED   %><%]];%>
<%(void)[[?%>#define WCONTINUED  <%=d WCONTINUED %><%]];%>
<%(void)[[?%>#define WNOWAIT     <%=d WNOWAIT    %><%]];%>

typedef enum {
  P_ALL  = <%=d P_ALL  %>,
  P_PID  = <%=d P_PID  %>,
  P_PGID = <%=d P_PGID %>
} idtype_t;
