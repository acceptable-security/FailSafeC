AC_DEFUN([FSC_AC_PATH_PREPEND],[
case "$2" in
	/*) $1="$2";;
	.) $1="$3";;
	*)
	case "$3" in
	.|./) $1="$2";;
	*/) $1="$3$2";;
	*) $1="$3/$2";;
	esac;;
esac
])
