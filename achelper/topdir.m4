AC_DEFUN([FSC_ARG_WITH_ARCH],
[
  AC_ARG_WITH(
    [arch],
    [AS_HELP_STRING([--with-arch=ARCH], [Specify architecture name [ARCH=TARGET]])],
    [
      AS_IF(
        [test x"$withval" != xyes && test x"$withval" != xno && test x"$withval" != x],
        [
          arch_alias=$withval
          arch=`$SHELL "$ac_aux_dir/config.sub" $arch_alias` ||
            AC_MSG_ERROR([Failed to canonicalize architecture name.])
        ],
        [AC_MSG_ERROR([Do not use --with-arch without architecture name.])]
      )
    ],
    [
      arch_alias=$target_alias
      arch=$target
    ]
  )
  AC_SUBST([arch_alias])
  AC_SUBST([arch])
])
