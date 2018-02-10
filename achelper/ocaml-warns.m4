AC_DEFUN([FSC_CHECK_OCAMLC_WARNOPTS],[
AC_MSG_CHECKING(m4_ifval([$3],[$3],[for available OCaml warning options]))
[$1="`ocamlc -help 2>&1 | sed -n '/^ *-w /,/^ *-/s/^ *\([$2]\)\/[a-z] .*/\1/p' | tr -d '\n'``ocamlc -help 2>&1 | sed -n '/^ *-w /,/^ *-/s/^ *[A-Z]\/\([$2]\) .*/\1/p' | tr -d '\n'`"]
AC_MSG_RESULT([$$1])
AC_SUBST([$1])
])

