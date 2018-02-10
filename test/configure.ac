#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(FSC_ACINIT_ARGS)
FSC_INIT()
AC_CONFIG_SRCDIR([../driver/fscc])
AC_CONFIG_AUX_DIR([../achelper])
AC_CONFIG_MACRO_DIR([../achelper])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

AC_ARG_ENABLE(
  [reautoconf],
  [AS_HELP_STRING([--disable-reautoconf], [Do not automatically invoke autoconf])],
  [enable_reautoconf="$enableval"], [enable_reautoconf=yes])
AC_SUBST([enable_reautoconf])

# Checks for programs.
AC_PROG_CC
AC_PROG_MKDIR_P

build_cross_compiler=native
AS_IF(
  [test x"$cross_compiling" = x"yes"], 
  [build_cross_compiler=cross],
  AS_IF(
    [test x"$cross_compiling" = x"no"],
    [build_cross_compiler=native],
    [build_cross_compiler=unknown]
  )
)
AC_SUBST([build_cross_compiler])

FSC_CC_OPTION([], [CC_CFLAGS], [-std=c99])
FSC_CC_OPTION([], [CC_CFLAGS], [-trigraphs])
FSC_CC_OPTION([], [CC_CFLAGS], [-O2])
AC_SUBST([CC_CFLAGS])

# Checks for libraries.
FSC_CHECK_LIBM
FSC_CHECK_LIBCRYPT
FSC_CHECK_LIBNDBM

# Checks for header files.

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
