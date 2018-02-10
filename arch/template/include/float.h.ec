<%# /* -*- c -*- */
#include <float.h>
#%>
<%#cflags -std=c99 #%>
/**
 * @file include/limits.h
 */
#ifndef __FLOAT_H
#define __FLOAT_H

#define FLT_RADIX <%=d FLT_RADIX %>

#define FLT_MANT_DIG  <%=d FLT_MANT_DIG  %>
#define DBL_MANT_DIG  <%=d DBL_MANT_DIG  %>
#define LDBL_MANT_DIG <%=d LDBL_MANT_DIG %>

#define DECIMAL_DIG   <%=d DECIMAL_DIG %>

#define FLT_DIG   <%=d FLT_DIG  %>
#define DBL_DIG   <%=d DBL_DIG  %>
#define LDBL_DIG  <%=d LDBL_DIG %>

#define FLT_MIN_EXP   <%=d FLT_MIN_EXP %>
#define DBL_MIN_EXP   <%=d DBL_MIN_EXP %>
#define LDBL_MIN_EXP  <%=d LDBL_MIN_EXP %>

#define FLT_MAX_EXP   <%=d FLT_MAX_EXP %>
#define DBL_MAX_EXP   <%=d DBL_MAX_EXP %>
#define LDBL_MAX_EXP  <%=d LDBL_MAX_EXP %>

#define FLT_MAX_10_EXP   <%=d FLT_MAX_10_EXP %>
#define DBL_MAX_10_EXP   <%=d DBL_MAX_10_EXP %>
#define LDBL_MAX_10_EXP  <%=d LDBL_MAX_10_EXP %>

#define FLT_MAX <%=.8e FLT_MAX %>F
#define DBL_MAX <%=.16e DBL_MAX %>
#define LDBL_MAX <%=.20Le LDBL_MAX %>L

#define FLT_EPSILON <%=.8e FLT_EPSILON %>F
#define DBL_EPSILON <%=.16e DBL_EPSILON %>
#define LDBL_EPSILON <%=.20Le LDBL_EPSILON %>L

#define FLT_MIN <%=.8e FLT_MIN %>F
#define DBL_MIN <%=.16e DBL_MIN %>
#define LDBL_MIN <%=.20Le LDBL_MIN %>L

#endif
