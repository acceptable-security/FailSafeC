<%# /* -*- c -*- */
#include <locale.h>
#%>
/**
 * @file include/locale.h
 */

#ifndef __LOCALE_H
#define __LOCALE_H

#ifndef NULL
#define NULL 0
#endif

#define LC_ALL      <%=d LC_ALL      %>
#define LC_COLLATE  <%=d LC_COLLATE  %>
#define LC_CTYPE    <%=d LC_CTYPE    %>
#define LC_MESSAGES <%=d LC_MESSAGES %>
#define LC_MONETARY <%=d LC_MONETARY %>
#define LC_NUMERIC  <%=d LC_NUMERIC  %>
#define LC_TIME     <%=d LC_TIME     %>

struct __fsc_attribute__((named "stdlib_lconv")) lconv {
	char *decimal_point;
	char *thousands_sep;
	char *grouping;
	char *int_curr_symbol;
	char *currency_symbol;
	char *mon_decimal_point;
	char *mon_grouping;
	char *positive_sign;
	char *negative_sign;
	char int_frac_digits;
	char frac_digits;
	char p_cs_precedes;
	char p_sep_by_space;
	char n_cs_precedes;
	char n_sep_by_space;
	char p_sign_posn;
	char n_sign_posn;

	char int_p_cs_precedes;
	char int_n_cs_precedes;
	char int_p_sep_by_space;
	char int_n_sep_by_space;
	char int_p_sign_posn;
	char int_n_sign_posn;
};

extern char *setlocale(int, const char *);
extern struct lconv *localeconv(void);

#endif
