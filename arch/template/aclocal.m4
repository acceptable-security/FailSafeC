# generated automatically by aclocal 1.9.6 -*- Autoconf -*-

# Copyright (C) 1996, 1997, 1998, 1999, 2000, 2001, 2002, 2003, 2004,
# 2005  Free Software Foundation, Inc.
# This file is free software; the Free Software Foundation
# gives unlimited permission to copy and/or distribute it,
# with or without modifications, as long as this notice is preserved.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY, to the extent permitted by law; without
# even the implied warranty of MERCHANTABILITY or FITNESS FOR A
# PARTICULAR PURPOSE.

m4_define([FSC_VERSION],[1.6.0])
# format of the above line should not be changed.
m4_define([FSC_ACINIT_ARGS],[Fail-Safe C,]FSC_VERSION[,fsc-contact@m.aist.go.jp,FailSafeC])
AC_DEFUN([FSC_INIT],[])

AC_DEFUN([FSC_CHECK_NDBM_H],
[
  AC_CHECK_HEADERS([ndbm.h], [ndbm_h_found='yes'])
  unset ac_cv_header_gdbm_ndbm_h
  AC_CHECK_HEADER([gdbm-ndbm.h], 
    [
      AC_DEFINE([HAVE_GDBM_DASH_NDBM_H], [1], [Define to 1 if you have the <gdbm-ndbm.h> header file.])
      ndbm_h_found='yes'
    ])
  unset ac_cv_header_gdbm_ndbm_h
  AC_CHECK_HEADER([gdbm/ndbm.h], 
    [
      AC_DEFINE([HAVE_GDBM_SLASH_NDBM_H], [1], [Define to 1 if you have the <gdbm/ndbm.h> header file.])
      ndbm_h_found='yes'
    ])
  AS_IF(
    [test x"$ndbm_h_found" != 'xyes'],
    AC_MSG_ERROR([ndbm header file was not found!])
  )
])

AC_DEFUN([FSC_CHECK_LIBNDBM],
[
  AC_CHECK_LIB([ndbm], [dbm_open], 
     [
      AC_DEFINE([HAVE_LIBNDBM], [1], [Define to 1 if you have the libndbm.a library.])
      [LIBS="-lndbm $LIBS"],
      ndbm_a_found='yes'
     ])
  AC_CHECK_LIB([gdbm_compat], [dbm_open],
    [
      AC_DEFINE([HAVE_LIBGDBM_COMPAT], [1], [Define to 1 if you have the libgdbm_compat.a library.])
      LIBS="-lgdbm_compat -lgdbm $LIBS"
      ndbm_a_found='yes'
    ], [], [-lgdbm])
  AS_IF(
    [test x"$ndbm_a_found" != 'xyes'],
    AC_MSG_ERROR([ndbm header file was not found!])
  )
])


