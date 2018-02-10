AC_DEFUN([AC_WITH_SAVED_VAR],[
ac_saved_$1="$$1"
$2
$1="$ac_saved_$1"
])
