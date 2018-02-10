# checking for gcc -iquote option
#
# test should be AC_LINK instead of AC_COMPILE;
#
#  gcc-2.95 accepts "gcc -c -iquote . test.c" as follows:
#    "-iquote" passed to cpp and ignored;
#    "." treated as linker script and ignored because of "-c";
#    "test.c" treated as input file.
#
AC_DEFUN([FSC_TEST_CC_IQUOTE_STYLE],[
    FSC_CC_IQUOTE_STYLE=
    fsc_save_CFLAGS="$CFLAGS"
    CFLAGS="-iquote ."
    AC_MSG_CHECKING([whether $[CC] $[CFLAGS] works])
    AC_LINK_IFELSE([AC_LANG_PROGRAM()],
      [AC_MSG_RESULT([yes])
       FSC_CC_IQUOTE_STYLE="-iquote"
      ],
      [AC_MSG_RESULT([no])
      CFLAGS="-I. -I-"
      AC_MSG_CHECKING([whether $[CC] $[CFLAGS] works])
      AC_LINK_IFELSE([AC_LANG_PROGRAM()],
        [AC_MSG_RESULT([yes])
        FSC_CC_IQUOTE_STYLE="-I-"],
        [AC_MSG_RESULT([no])])])
   CFLAGS="$fsc_save_CFLAGS"
   ])
AC_SUBST([FSC_CC_IQUOTE_STYLE])
