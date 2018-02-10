#                                               -*- Autoconf -*-
# Process this file with autoconf to produce a configure script.

AC_PREREQ(2.61)
AC_INIT(FSC_ACINIT_ARGS)
FSC_INIT()
AC_CONFIG_SRCDIR([fsc_runtime.h])
AC_CONFIG_AUX_DIR([../achelper])
AC_CONFIG_MACRO_DIR([../achelper])
AC_CONFIG_HEADERS([fsc_autoconf.h])
AC_CANONICAL_BUILD
AC_CANONICAL_HOST
AC_CANONICAL_TARGET

# disable default CFLAGS '-g -O2'
CFLAGS=$CFLAGS

# Checks for programs.
AC_PROG_CC
FSC_PROG_AR
FSC_PROG_OBJDUMP
AC_PROG_LN_S
AC_PROG_MKDIR_P

FSCC_CFLAGS_OPT_OFLAG=
FSCC_CFLAGS_OPT=
FSCC_CFLAGS_DEBUG=

# check for optimizing/code-generation option
FSC_CC_OPTION_FIRST([], [FSCC_CFLAGS_OPT_OFLAG], [-O6, -O3, -O2, -O1, -O])
FSC_CC_OPTION_ALL([], [FSCC_CFLAGS_OPT_OFLAG], [-g])
FSC_CC_OPTION_ALL([FSCC_CFLAGS_OPT_OFLAG], [FSCC_CFLAGS_OPT], [-fno-strict-aliasing, -Wall, -Wno-unused])
FSC_CC_OPTION_ALL([], [FSCC_CFLAGS_DEBUG], [-g, -O0, -fno-strict-aliasing, -Wall, -Wno-unused])

AC_SUBST([FSCC_CFLAGS_OPT])
AC_SUBST([FSCC_CFLAGS_OPT_OFLAG])
AC_SUBST([FSCC_CFLAGS_DEBUG])

AC_ARG_ENABLE(
  [reautoconf],
  [AS_HELP_STRING([--disable-reautoconf], [Do not automatically invoke autoconf])],
  [enable_reautoconf="$enableval"], [enable_reautoconf=yes])
AC_SUBST([enable_reautoconf])

# check for tuning option
FSC_DEFAULT_TUNE_OPTIONS

# check for other cc options
FSC_TEST_CC_IQUOTE_STYLE

# Checks for libraries.
FSC_CHECK_LIBM
FSC_WITH_GC

AC_WITH_SAVED_VAR([LIBS],[
FSC_CHECK_LIBCRYPT
FSC_CHECK_LIBNDBM
])

AC_CHECK_FUNCS(backtrace)

# Checks for header files.
FSC_CHECK_NDBM_H

# Checks for typedefs, structures, and compiler characteristics.

# Checks for library functions.

FSC_PERLIFY(CC_PL, CC)
FSC_PERLIFY(FSCC_CFLAGS_OPT_OFLAG_PL, FSCC_CFLAGS_OPT_OFLAG)
FSC_PERLIFY(FSCC_CFLAGS_OPT_PL, FSCC_CFLAGS_OPT)
FSC_PERLIFY(FSCC_CFLAGS_DEBUG_PL, FSCC_CFLAGS_DEBUG)
FSC_PERLIFY(CPPFLAGS_PL, CPPFLAGS)
FSC_PERLIFY(LDFLAGS_PL, LDFLAGS)
FSC_PERLIFY(LIBS_PL, LIBS)
FSC_PERLIFY(FSCC_GCC_ARCH_DEFAULT_PL, FSCC_GCC_ARCH_DEFAULT)
FSC_PERLIFY(FSCC_GCC_TUNE_DEFAULT_PL, FSCC_GCC_TUNE_DEFAULT)

AC_CONFIG_FILES([Makefile])
AC_CONFIG_FILES([Makefile.runtime.rules])
AC_CONFIG_FILES([libs/Makefile])
AC_CONFIG_FILES([libs/libfsc/Makefile])
AC_CONFIG_FILES([libs/libndbm/Makefile])
AC_CONFIG_FILES([libs/libmmap_slow/Makefile])
AC_CONFIG_FILES([stdlib/Makefile])
AC_CONFIG_FILES([stdlib/bsd/Makefile])
AC_CONFIG_FILES([stdlib/bsdsource/Makefile])
AC_CONFIG_FILES([stdlib/fscw/Makefile])
AC_CONFIG_FILES([stdlib/unistd/Makefile])
AC_CONFIG_FILES([fscc.conf])
AC_CONFIG_FILES(
  [../tools/read-elf-section],
  [chmod +x ../tools/read-elf-section]
)
AC_OUTPUT
