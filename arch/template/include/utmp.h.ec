<%# /* -*- c -*- */
#include <utmp.h>
#%>
/**
 * @file include/utmp.h
 */
#ifndef __UTMP_H
#define __UTMP_H

#include <stdint.h>
#include <sys/time.h>
#include <sys/types.h>

#define UT_LINESIZE 32
#define UT_NAMESIZE 32
#define UT_HOSTSIZE 32

struct __fsc_attribute__((named "stdlib_lastlog")) lastlog {
  time_t ll_time;
  char ll_line[UT_LINESIZE];
  char ll_host[UT_HOSTSIZE];
};

struct __fsc_attribute__((named "stdlib_exit_status")) exit_status {
  short e_termination;
  short e_exit;
};

struct __fsc_attribute__((named "stdlib_utmp")) utmp {
  short ut_type;
  pid_t ut_pid;
  char ut_line[UT_LINESIZE];
  char ut_id[4];
  char ut_user[UT_NAMESIZE];
  char ut_host[UT_HOSTSIZE];
  struct exit_status ut_exit;
  long ut_session;
  struct timeval ut_tv;
  int32_t ut_addr_v6[4];
  char __unused[20];
};

#define ut_name ut_user
#define ut_time ut_tv.tv_sec
#define ut_addr ut_addr_v6[0]

#define EMPTY         <%=d EMPTY         %>
#define RUN_LVL       <%=d RUN_LVL       %>
#define BOOT_TIME     <%=d BOOT_TIME     %>
#define NEW_TIME      <%=d NEW_TIME      %>
#define OLD_TIME      <%=d OLD_TIME      %>
#define INIT_PROCESS  <%=d INIT_PROCESS  %>
#define LOGIN_PROCESS <%=d LOGIN_PROCESS %>
#define USER_PROCESS  <%=d USER_PROCESS  %>
#define DEAD_PROCESS  <%=d DEAD_PROCESS  %>
#define ACCOUNTING    <%=d ACCOUNTING    %>

#endif

