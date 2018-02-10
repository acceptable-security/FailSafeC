AC_DEFUN([FSC_CHECK_OCAMLC],
[
  AC_MSG_CHECKING([whether ocamlc and ocamlfind works])
  AS_IF(
    [ocamlfind ocamlc >/dev/null 2>/dev/null],
    [AC_MSG_RESULT([yes])],
    [
      AC_MSG_RESULT([no])
      AC_MSG_ERROR([Cannot find useful ocamlfind or ocamlc:  check Objective Caml installation.])
    ])
])

AC_DEFUN([FSC_CHECK_OCAMLOPT],
[
  AC_MSG_CHECKING([whether ocamlopt works])
  AS_IF(
    [ocamlfind ocamlopt >/dev/null 2>/dev/null],
    [AC_MSG_RESULT([yes])],
    [
      AC_MSG_RESULT([no])
      AC_SUBST([OCAML_BYTECODE_ONLY], [1])
    ])
])

