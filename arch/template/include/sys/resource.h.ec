<%# /* -*- c -*- */
#include <sys/resource.h>
#%>
/**
 * @file include/sys/resource.h
 */

#ifndef __SYS_RESOURCE_H
#define __SYS_RESOURCE_H

#include <sys/__types.h>

#ifndef __ID_T
#define __ID_T
typedef __id_t id_t;
#endif

#ifndef __RLIM_T
#define __RLIM_T
typedef __rlim_t rlim_t;
#endif

#include <sys/time.h>

#define PRIO_PROCESS <%=d PRIO_PROCESS %>
#define PRIO_PGRP    <%=d PRIO_PGRP    %>
#define PRIO_USER    <%=d PRIO_USER    %>

#define RLIM_INFINITY  <%=d RLIM_INFINITY  %>
#define RLIM_SAVED_MAX <%=d RLIM_SAVED_MAX %>
#define RLIM_SAVED_CUR <%=d RLIM_SAVED_CUR %>

#define RUSAGE_SELF     <%=d RUSAGE_SELF     %>
#define RUSAGE_CHILDREN <%=d RUSAGE_CHILDREN %>

#define RLIMIT_CORE   <%=d RLIMIT_CORE   %>
#define RLIMIT_CPU    <%=d RLIMIT_CPU    %>
#define RLIMIT_DATA   <%=d RLIMIT_DATA   %>
#define RLIMIT_FSIZE  <%=d RLIMIT_FSIZE  %>
#define RLIMIT_NOFILE <%=d RLIMIT_NOFILE %>
#define RLIMIT_STACK  <%=d RLIMIT_STACK  %>
#define RLIMIT_AS     <%=d RLIMIT_AS     %>

struct __fsc_attribute__((named "stdlib_rlimit")) rlimit {
  rlim_t rlim_cur;
  rlim_t rlim_max;
};

struct __fsc_attribute__((named "stdlib_rusage")) rusage {
  struct timeval ru_utime;
  struct timeval ru_stime;
};

extern int getpriority(int, id_t);
extern int getrlimit(int, struct rlimit *);
extern int getrusage(int, struct rusage *);
extern int setpriority(int, id_t, int);
extern int setrlimit(int, const struct rlimit *);

#endif
