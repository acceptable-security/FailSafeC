AC_DEFUN([FSC_WITH_GC],
[
  AC_ARG_WITH(
    [gc],
    [AS_HELP_STRING([--with-gc=PATH], [Use libgc in PATH])],
    [
      if test "x$withval" == xyes ; then
        :
      elif test "x$withval" != xno && test "x$withval" != x ; then
        if test -d $withval/include ; then
          CPPFLAGS="$CPPFLAGS -I$withval/include"
        else
          CPPFLAGS="$CPPFLAGS -I$withval"
        fi 
        if test -d $withval/lib ; then
          LDFLAGS="$LDFLAGS -L$withval/lib"
        else
          LDFLAGS="$LDFLAGS -L$withval"
        fi
      fi
    ]
  )

  AC_CHECK_HEADERS(
    [gc.h],
    [],
    [AC_CHECK_HEADERS(
      [gc/gc.h],
      [],
      AC_MSG_ERROR([gc.h was not found. use --with-gc])
      )])
  AC_CHECK_LIB(
    [gc],
    [GC_malloc],
    [],
    AC_MSG_ERROR([libgc was not found. use --with-gc])
    )
])

