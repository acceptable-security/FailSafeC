<%#shared
#include <errno.h>
#%>
<%#libs -lm #%>
<%#cflags -Wall #%>
#define EINVAL <%=d EINVAL %>
#define ESOMETHING <%[[%><%#onerror (-1)#%><%=d ESOMETHING %><%]]%>
#define EFAULT <%=d EFAULT %>
