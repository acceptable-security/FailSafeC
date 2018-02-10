#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(FSC_ACINIT_ARGS)
FSC_INIT()
AC_CONFIG_AUX_DIR(["$srcdir/../../achelper"])
#AC_CONFIG_MACRO_DIR(["$srcdir/../../achelper"]) aclocal's -I option should be used with absolute paths
AC_CONFIG_SRCDIR([fsc_config.ml.ec])
AC_CONFIG_HEADERS([config.h])

AC_ARG_ENABLE(
  [reautoconf],
  [AS_HELP_STRING([--disable-reautoconf], [Do not automatically invoke autoconf])],
  [enable_reautoconf="$enableval"], [enable_reautoconf=yes])
AC_SUBST([enable_reautoconf])

# Checks for programs.
AC_PROG_MKDIR_P
AC_PROG_INSTALL

# Checks for libraries.

# Checks for header files.
FSC_CHECK_NDBM_H

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

AC_CONFIG_FILES([Makefile])
AC_OUTPUT
