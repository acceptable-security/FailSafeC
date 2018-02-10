AC_DEFUN([FSC_DEFAULT_TUNE_OPTIONS],[tuneexplicit=no
case "$target" in
  i686-*)
    tunetest=i386
    tunetest_direct=-m386
    tunedefault=pentium4
    ;;
  i[[345]]86-*)
    tunetest=i386
    tunetest_direct=-m386
    tunedefault=no
    ;;
  *)
    tunetest=no
    tunetest_direct=
    tunedefault=no
    ;;
esac

AC_ARG_ENABLE(
	[default-tuning],
	[AS_HELP_STRING([--enable-default-tuning[=arch]], [Specify default tuning options for gcc])],
	[
	  case x"$enableval" in
	    xyes|x)
	      if test "x$tunedefault" = "xno"; then
	        echo "no default value for --enable-default-tuning is set for $target " 1>&2;
		exit 1;
	      fi
	      ;;
	    xno)
	      tunedefault=no
	      ;;
	    x*)
	      tunedefault="$enableval"
	      AS_IF([ test "$tunetest" = "no" ], [ tunetest="$tunedefault" ], [])
	      tuneexplicit=yes
	      ;;
	  esac
	],[])

#echo "arch=$target tunetest=$tunetest tunetest_direct=$tunetest_direct tunedefault=$tunedefault"

case "x$tunetest" in
  x|xno)
  FSCC_GCC_TUNE_FLAG=
  FSCC_GCC_ARCH_FLAG=
  FSCC_TUNE_DEFAULT=
  ;;
  *)
  FSC_CC_OPTION_FIRST([], [fsc_tune_flag], [-mtune=$tunetest, -mcpu=$tunetest])
  if test "x$fsc_tune_flag" = "x" -a "x$tunetest_direct" != "x"; then
    FSC_CC_OPTION_FIRST([], [fsc_tune_flag], [$tunetest_direct])
  fi
  case "x$fsc_tune_flag" in
  x)
    ;;
  x-mtune=*)
    FSCC_GCC_TUNE_FLAG="-mtune="
    FSCC_GCC_ARCH_FLAG="-march="
    ;;
  x-mcpu=*)
    FSCC_GCC_TUNE_FLAG="-mcpu="
    FSCC_GCC_ARCH_FLAG="-march="
    ;;
  x-m*)
    FSCC_GCC_TUNE_FLAG="-m"
    FSCC_GCC_ARCH_FLAG="-m"
    case "$tunedefault" in
    i386) tunedefault=386 ;;
    i486) tunedefault=486 ;;
    esac
  esac
  FSCC_GCC_ARCH_DEFAULT=
  FSCC_GCC_TUNE_DEFAULT=
  AS_IF([ test "x$fsc_tune_flag" != "x" -a "x$tunedefault" != "xno" ],[
    FSC_CC_OPTION_TRY([], [FSCC_GCC_ARCH_DEFAULT], "$FSCC_GCC_ARCH_FLAG$tunedefault",[
	FSC_CC_OPTION_TRY([], [FSCC_GCC_TUNE_DEFAULT], "$FSCC_GCC_TUNE_FLAG$tunedefault")
      ],[
      AS_IF([ test "$tuneexplicit" = yes ], [
        echo "Warning: the compiler do not accept architecture specified in --enable-default-tuning"
	],[])])
    ],[])
  ;;
esac
#echo "tuneflag=$FSCC_GCC_TUNE_FLAG tunedefault=$FSCC_TUNE_DEFAULT"
AC_SUBST([FSCC_GCC_TUNE_FLAG])
AC_SUBST([FSCC_GCC_ARCH_DEFAULT])
AC_SUBST([FSCC_GCC_TUNE_DEFAULT])
])
