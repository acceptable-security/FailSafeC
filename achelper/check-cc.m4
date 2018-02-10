# FSC_CC_OPTION_IFELSE([pre], [option], [iftrue], [ifelse])
#   try compiling with $CC <pre> <option>, execute one of branches
AC_DEFUN([FSC_CC_OPTION_IFELSE],
[fsc_save_CFLAGS="$CFLAGS"
if test -z "$1"; then CFLAGS="$2"; else CFLAGS="$1 $2"; fi
AC_MSG_CHECKING([whether $[CC] $[CFLAGS] works])
AC_COMPILE_IFELSE([AC_LANG_PROGRAM()],
  [AC_MSG_RESULT([yes])
   CFLAGS="$fsc_save_CFLAGS"
  $3],
  [AC_MSG_RESULT([no])
   CFLAGS="$fsc_save_CFLAGS"
  $4])
])

# FSC_CC_OPTION_TRY([pre], [option], [testing-option], [iftrue], [ifelse])
# try compiling with $CC $PRE $OPTION <testing-option>.
#  if successful, append <testing-option> to $OPTION and execute <iftrue>.
#  if unsuccessful, execute <ifelse>.
# <pre> and <option> are environmental variable names, <testing-option> is a string.
AC_DEFUN([FSC_CC_OPTION_TRY],
  [FSC_CC_OPTION_IFELSE([m4_foreach([var],[$1],${var}${var:+${$2:+ }})${$2}],[$3],[
	$2="${$2}${$2:+ }$3"
	$4],[$5])])

# FSC_CC_OPTION_TRY([pre], [option], [testing-option])
# try compiling with $CC $PRE $OPTION <testing-option>.
#  if successful, append <testing-option> to $OPTION.
AC_DEFUN([FSC_CC_OPTION],[FSC_CC_OPTION_TRY([$1],[$2],[$3],[],[])])

# FSC_CC_OPTION_TRY([pre], [option], [testing-options])
# find first option accepted by the compiler and append it to $OPTION.
AC_DEFUN([FSC_CC_OPTION_FIRST],
[m4_ifval(
  [$3],
  [FSC_CC_OPTION_IFELSE([m4_foreach([var],[$1],${var}${var:+${$2:+ }})${$2}], m4_car($3),
       [$2="${$2}${$2:+ }m4_car($3)"
        ],
       [FSC_CC_OPTION_FIRST([$1], [$2], m4_cdr($3))])],
  [])])

# FSC_CC_OPTION_ALL([pre], [option], [testing-options])
# append all option saccepted by the compiler to $OPTION.
AC_DEFUN([FSC_CC_OPTION_ALL],
[m4_ifval(
  [$3],
  [FSC_CC_OPTION_IFELSE([m4_foreach([var],[$1],${var}${var:+${$2:+ }})${$2}], m4_car($3),
       [$2="${$2}${$2:+ }m4_car($3)"
        ],
       [])
   FSC_CC_OPTION_ALL([$1], [$2], m4_cdr($3))],
  [])])

# FSC_CC_OPTION_ALL_1BY1([pre], [option], [testing-options])
# put all option accepted by the compiler (trying one by one) to $OPTION.
AC_DEFUN([FSC_CC_OPTION_ALL_1BY1],
[m4_ifval(
  [$3],
  [FSC_CC_OPTION_IFELSE([m4_foreach([var],[$1],${var}${var:+${$2:+ }})], m4_car($3),
       [$2="${$2}${$2:+ }m4_car($3)"
        ],
       [])
   FSC_CC_OPTION_ALL_1BY1([$1], [$2], m4_cdr($3))],
  [])])

