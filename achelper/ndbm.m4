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

