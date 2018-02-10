<%#cflags -D_XOPEN_SOURCE=500 #%>
<%# /* -*- c -*- */
#include <limits.h>
#%>
/**
 * @file include/limits.h
 */
#ifndef __LIMITS_H
#define __LIMITS_H

#define PATH_MAX <%=d PATH_MAX %>
#define LOGIN_NAME_MAX <%=d LOGIN_NAME_MAX %>

#define CHAR_BIT 8

#define CHAR_MIN     <%=d CHAR_MIN %>
#define CHAR_MAX     <%=d CHAR_MAX %>
#define UCHAR_MAX    255
#define USHRT_MAX    65535
#define UINT_MAX     4294967295U
#define ULONG_MAX    4294967295UL
#define ULLONG_MAX   18446744073709551615ULL
#define SCHAR_MAX    127
#define SCHAR_MIN    (-SCHAR_MIN-1)
#define SHRT_MAX     32767
#define SHRT_MIN     (-SHRT_MAX-1)
#define INT_MAX      2147483647
#define INT_MIN      (-INT_MAX-1)
#define LONG_MAX     2147483647
#define LONG_MIN     (-LONG_MAX-1)
#define LLONG_MAX    9223372036854775807LL
#define LLONG_MIN    (-LLONG_MAX-1)

#define SIZE_MAX UINT_MAX
#define SSIZE_MAX INT_MAX
#define SSIZE_MIN INT_MIN

#define IOV_MAX <%=d IOV_MAX %>

#define BC_BASE_MAX        <%=d BC_BASE_MAX        %>
#define BC_DIM_MAX         <%=d BC_DIM_MAX         %>
#define BC_SCALE_MAX       <%=d BC_SCALE_MAX       %>
#define BC_STRING_MAX      <%=d BC_STRING_MAX      %>
#define CHARCLASS_NAME_MAX <%=d CHARCLASS_NAME_MAX %>
#define COLL_WEIGHTS_MAX   <%=d COLL_WEIGHTS_MAX   %>
#define EXPR_NEST_MAX      <%=d EXPR_NEST_MAX      %>
#define LINE_MAX           <%=d LINE_MAX           %>
#define NGROUPS_MAX        <%=d NGROUPS_MAX        %>
#define RE_DUP_MAX         <%=d RE_DUP_MAX         %>

#endif
