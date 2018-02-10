<%# /* -*- c -*- */
#include <sys/uio.h>
 #%>
/**
 * @file include/sys/uio.h
 */
#ifndef __SYS_UIO_H
#define __SYS_UIO_H

#include <sys/__types.h>

#ifndef __SIZE_T
#define __SIZE_T
typedef __size_t size_t;
#endif

#ifndef __SSIZE_T
#define __SSIZE_T
typedef __ssize_t ssize_t;
#endif

#define UIO_MAXIOV <%=d UIO_MAXIOV %>

struct __fsc_attribute__((named "stdlib_iovec")) iovec {
  void *iov_base;
  size_t iov_len;
};

extern ssize_t writev(int, const struct iovec *, int);
extern ssize_t readv(int, const struct iovec *, int);

#endif

