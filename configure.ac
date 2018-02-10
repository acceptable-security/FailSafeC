#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(FSC_ACINIT_ARGS)
FSC_INIT()
AC_CONFIG_SRCDIR([driver/fscc])
AC_CONFIG_AUX_DIR([achelper])
AC_CONFIG_MACRO_DIR([achelper])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

runtime_configure_opt=''
test_configure_opt=''

AC_ARG_ENABLE(
  [reautoconf],
  [AS_HELP_STRING([--enable-reautoconf], [Enable automatic autoconf invocation])],
  [runtime_configure_opt="$runtime_configure_opt --enable-reautoconf='$enableval'"
   test_configure_opt="$test_configure_opt --enable-reautoconf='$enableval'"
   enable_reautoconf="$enableval"],
   [if test -d ./.svn; then 
     enable_reautoconf=yes
    else
     enable_reautoconf=no
    fi
    ])
AC_SUBST([enable_reautoconf])

AC_ARG_WITH(
  [gc],
  [AS_HELP_STRING([--with-gc=PATH], [Use libgc in PATH])],
  [runtime_configure_opt="$runtime_configure_opt --with-gc='$withval'"], [])
AC_ARG_ENABLE(
  [default-tuning],
  [AS_HELP_STRING([--enable-default-tuning[=arch]], [Specify default tuning options for gcc])],
  [runtime_configure_opt="$runtime_configure_opt --enable-default-tuning='$enableval'"], [])

AC_SUBST([runtime_configure_opt])
AC_SUBST([test_configure_opt])

FSC_ARG_WITH_ARCH

AC_MSG_CHECKING([whether building cross compiler])
build_cross_compiler=native
AS_IF([test x"$target_alias" != x"" && test x"$target" != x"$host"],
  [AC_MSG_RESULT(yes); build_cross_compiler=cross; fsc_target=$target_alias],
  [AC_MSG_RESULT(no); fsc_target=default])
AC_SUBST([fsc_target])

# Checks for programs.
AC_PROG_CC
AC_PROG_LN_S
AC_PROG_MKDIR_P
AC_PROG_INSTALL
FSC_CHECK_OCAMLC
FSC_CHECK_OCAMLOPT

FSC_CHECK_OCAMLC_WARNOPTS(OCAMLC_WARNS, Aexyz)

# Checks for libraries.

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_SUBST([build_cross_compiler])

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([Makefile.ocamlc-common])
AC_CONFIG_FILES([common/Makefile])
AC_CONFIG_FILES([parser/Makefile])
AC_CONFIG_FILES([trans/Makefile])
AC_CONFIG_FILES([linker/Makefile])
AC_CONFIG_FILES([fscw/Makefile])
AC_CONFIG_FILES([doc/Makefile])

AC_CONFIG_SUBDIRS([runtime test])

AC_OUTPUT

# this depends on existence of AC_CONFIGURE_SUBDIRS (of autoconf 2.61)
if test "$no_recursion" != yes; then

AC_MSG_NOTICE([preparing for architecture-specific files in arch-$target])
FSC_AC_PATH_PREPEND([top_src_dir],[$srcdir],[..])
case $build_cross_compiler in
     native)
	$MKDIR_P arch-$target
	FSC_AC_PATH_PREPEND([sub_cache_file],[$cache_file],[$top_src_dir])
	AC_MSG_NOTICE([running $top_src_dir/arch/template/configure $ac_sub_configure_args --cache-file='$sub_cache_file' --srcdir='$top_src_dir/arch/template'])
	eval "cd arch-$target && $top_src_dir/arch/template/configure $ac_sub_configure_args --cache-file='$sub_cache_file' --srcdir='$top_src_dir/arch/template'" ||
        AC_MSG_ERROR([configure failed for arch-$target])
	;;
     cross)
	test -d $srcdir/arch/$arch_alias ||
	AC_MSG_ERROR([no prepared configuration in $srcdir/arch/$arch_alias: make it in native environment])
	$LN_S $top_src_dir/arch/$arch_alias arch-$target
        ;;
     configure)
        # dummy rule
	AC_CONFIG_SUBDIRS([arch/template])
	;;
esac
fi

#AC_MSG_NOTICE([=== creating arch directory])
#make arch-dir-makefile
#AC_MSG_NOTICE([=== configuring runtime directory])
#make runtime-makefile
