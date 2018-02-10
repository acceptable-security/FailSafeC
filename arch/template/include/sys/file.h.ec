<%# /* -*- c -*- */
#include <sys/file.h>
#%>
/**
 * @file include/sys/file.h
 */
#ifndef __SYS_FILES_H
#define __SYS_FILES_H
#include <fcntl.h>

extern int flock(int, int);

#define LOCK_SH <%=d LOCK_SH %>
#define LOCK_EX <%=d LOCK_EX %>
#define LOCK_NB <%=d LOCK_NB %>
#define LOCK_UN <%=d LOCK_UN %>

#endif
