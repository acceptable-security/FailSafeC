<%# /* -*- c -*- */
#include <fcntl.h>
#%>
/**
 * @file include/fcntl_internal.h
 */

#define F_DUPFD		<%=d F_DUPFD  %>
#define F_GETFD		<%=d F_GETFD  %>
#define F_SETFD		<%=d F_SETFD  %>
#define F_GETFL		<%=d F_GETFL  %>
#define F_SETFL		<%=d F_SETFL  %>
#define F_GETLK		<%=d F_GETLK  %>
#define F_SETLK		<%=d F_SETLK  %>
#define F_SETLKW	<%=d F_SETLKW %>
#define F_GETOWN	<%=d F_GETOWN %>
#define F_SETOWN	<%=d F_SETOWN %>

#define FD_CLOEXEC	<%=d FD_CLOEXEC %>

#define F_RDLCK		<%=d F_RDLCK %>
#define F_UNLCK		<%=d F_UNLCK %>
#define F_WRLCK		<%=d F_WRLCK %>

#define O_CREAT		<%=d O_CREAT  %>
#define O_EXCL		<%=d O_EXCL   %>
#define O_NOCTTY	<%=d O_NOCTTY %>
#define O_TRUNC		<%=d O_TRUNC  %>

#define O_APPEND	<%=d O_APPEND   %>
#define O_DSYNC		<%=d O_DSYNC    %>
#define O_NONBLOCK	<%=d O_NONBLOCK %>
#define O_RSYNC		<%=d O_RSYNC    %>
#define O_SYNC		<%=d O_SYNC     %>

#define O_ACCMODE	<%=d O_ACCMODE %>

#define O_RDONLY	<%=d O_RDONLY %>
#define O_RDWR		<%=d O_RDWR   %>
#define O_WRONLY	<%=d O_WRONLY %>

/* non-standard */
#define O_NDELAY	<%=d O_NDELAY %>
#define FNDELAY         <%=d FNDELAY %>
