<%# /* -*- c -*- */
#include <endian.h>
#%>
/**
 * @file include/endian.h
 */
#ifndef __ENDIAN_H
#define __ENDIAN_H

#define __LITTLE_ENDIAN <%=d __LITTLE_ENDIAN %>
#define __BIG_ENDIAN    <%=d __BIG_ENDIAN %>

#define __BYTE_ORDER    <%=d __BYTE_ORDER %>

#define LITTLE_ENDIAN __LITTLE_ENDIAN
#define BIG_ENDIAN __BIG_ENDIAN
#define BYTE_ORDER __BYTE_ORDER

#endif
