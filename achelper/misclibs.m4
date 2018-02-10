AC_DEFUN([FSC_CHECK_LIBM],
[
  AC_CHECK_FUNC(
    [log],
    [],
    AC_CHECK_LIB(
      [m],
      [log],
      [
	LIBS="-lm $LIBS"
        AC_DEFINE([HAVE_LIBM], [1], [Define to 1 if you have the `m' library (-lm).])
      ],
      AC_MSG_ERROR([libm was not found.])
      )
    )
])

AC_DEFUN([FSC_CHECK_LIBCRYPT],
[
  AC_CHECK_FUNC(
    [crypt],
    [],
    AC_CHECK_LIB(
      [crypt],
      [crypt],
      [
      LIBS="-lcrypt $LIBS"
      AC_DEFINE([HAVE_LIBCRYPT], [1], [Define to 1 if you need libcrypt.a for crypt(3).])
      ],
      AC_MSG_ERROR([crypt(3) was not found.])
      )
    )
])

AC_DEFUN([FSC_SAVE_VAR],
[
  fsc_saved_$1="$1"
  $2
  $1=fsc_saved_$2
]
)
