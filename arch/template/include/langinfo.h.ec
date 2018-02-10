<%# /* -*- c -*- */
#include <langinfo.h>
#%>
/**
 * @file include/langinfo.h
 */
#ifndef __LANGINFO_H
#define __LANGINFO_H
#include <nl_types.h>

char *nl_langinfo(nl_item item);

#define CODESET     <%=d CODESET     %>
#define D_T_FMT     <%=d D_T_FMT     %>
#define D_FMT       <%=d D_FMT       %>
#define T_FMT       <%=d T_FMT       %>
#define T_FMT_AMPM  <%=d T_FMT_AMPM  %>
#define AM_STR      <%=d AM_STR      %>
#define PM_STR      <%=d PM_STR      %>
#define DAY_1       <%=d DAY_1       %>
#define DAY_2       <%=d DAY_2       %>
#define DAY_3       <%=d DAY_3       %>
#define DAY_4       <%=d DAY_4       %>
#define DAY_5       <%=d DAY_5       %>
#define DAY_6       <%=d DAY_6       %>
#define DAY_7       <%=d DAY_7       %>
#define ABDAY_1     <%=d ABDAY_1     %>
#define ABDAY_2     <%=d ABDAY_2     %>
#define ABDAY_3     <%=d ABDAY_3     %>
#define ABDAY_4     <%=d ABDAY_4     %>
#define ABDAY_5     <%=d ABDAY_5     %>
#define ABDAY_6     <%=d ABDAY_6     %>
#define ABDAY_7     <%=d ABDAY_7     %>
#define MON_1       <%=d MON_1       %>
#define MON_2       <%=d MON_2       %>
#define MON_3       <%=d MON_3       %>
#define MON_4       <%=d MON_4       %>
#define MON_5       <%=d MON_5       %>
#define MON_6       <%=d MON_6       %>
#define MON_7       <%=d MON_7       %>
#define MON_8       <%=d MON_8       %>
#define MON_9       <%=d MON_9       %>
#define MON_10      <%=d MON_10      %>
#define MON_11      <%=d MON_11      %>
#define MON_12      <%=d MON_12      %>
#define ABMON_1     <%=d ABMON_1     %>
#define ABMON_2     <%=d ABMON_2     %>
#define ABMON_3     <%=d ABMON_3     %>
#define ABMON_4     <%=d ABMON_4     %>
#define ABMON_5     <%=d ABMON_5     %>
#define ABMON_6     <%=d ABMON_6     %>
#define ABMON_7     <%=d ABMON_7     %>
#define ABMON_8     <%=d ABMON_8     %>
#define ABMON_9     <%=d ABMON_9     %>
#define ABMON_10    <%=d ABMON_10    %>
#define ABMON_11    <%=d ABMON_11    %>
#define ABMON_12    <%=d ABMON_12    %>
#define ERA         <%=d ERA         %>
#define ERA_D_FMT   <%=d ERA_D_FMT   %>
#define ERA_D_T_FMT <%=d ERA_D_T_FMT %>
#define ERA_T_FMT   <%=d ERA_T_FMT   %>
#define ALT_DIGITS  <%=d ALT_DIGITS  %>
#define RADIXCHAR   <%=d RADIXCHAR   %>
#define THOUSEP     <%=d THOUSEP     %>
#define YESEXPR     <%=d YESEXPR     %>
#define NOEXPR      <%=d NOEXPR      %>
#define CRNCYSTR    <%=d CRNCYSTR    %>

#endif
