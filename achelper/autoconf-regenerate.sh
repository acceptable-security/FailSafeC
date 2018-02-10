#!/bin/sh
test -d achelper && cd achelper

# to make that *.m4's are copied into aclocal.m4,
# not referenced by "m4_include" in absolute targets.
ACHELPER="`pwd`"

cd ..
set -e

test_curdir () {
    if test -f achelper/prog-ocaml.m4 -a -f driver/fscc; then
	:
    else
	echo "invoke it from top directory"
	exit 1
    fi
}

docmd () {
    echo "+" "$@"
    "$@" || exit 1
}

doit () {
    back="`echo $1 | sed -e 's/[a-zA-Z0-9][a-zA-Z0-9]*/../g'`"
    case "$2" in
	relative)
	    achelper="$back/achelper"
	    ;;
	absolute)
	    achelper="$ACHELPER"
	    ;;
    esac
    docmd cd $1
    docmd aclocal -I "$achelper"
    docmd autoconf
    docmd cd $back
    test_curdir
}

test_curdir

for dir in . runtime test; do
    doit $dir relative
done

# The script arch/template/configure may be copied before use;
# do not use relative paths inside it.
for dir in arch/template; do
    doit $dir absolute
done
