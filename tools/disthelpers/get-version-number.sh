#!/bin/sh
srcdir=.

if [ "$1" = -src ]; then
    srcdir="${2}"
    shift 2
fi

cd $srcdir

test -f "Makefile.in" || exit 1
test -f "driver/fscc" || exit 1

if test -s VERSION; then
    read ver < VERSION
    echo 2:"$ver"
    if test -z "$ver"; then
	echo "cannot get version number from file VERSION" >&2
	exit 1;
    fi
    verstr="$ver"
    rev="`echo \"$ver\" |
          sed -e 's/^..*\.\([0-9][0-9+TZ]*\)$/\1/g' \
	      -e '/^.*[^0-9+TZ].*$/d'`"
    rel="`echo \"$ver\" |
          sed -e 's/^\(..*\)\.[0-9][0-9+TZ]*$/\1/g' `"
    attr=""
elif test -d .svn; then
    rel="`head -1 \"achelper/fsc-version.m4\" | 
         sed -e 's/^m4_define(\[FSC_VERSION\],\[\([0-9][^]]*\)\]..*$/\1/g' \
             -e '/^..*[^a-z0-9~.]..*$/d'`"
    if test -z "$rel"; then
	echo "cannot determine release number" >&2
	exit 1;
    fi
    rev="`tools/disthelpers/svn-get-revision.pl`"
    attr=" (development)"
    ver="$rel.$rev"
    verstr="$ver (development)"
else
    echo "no way to guess version number" >&2
    exit 1;
fi

if test -z "$rev"; then
    echo "cannot determine revision number" >&2
    exit 1;
fi
if test "$rel.$rev" != "$ver"; then
    echo "cannot determine revision number" >&2
    exit 1;
fi

case $1 in
    -eval)
	echo "ver='$ver'"
	echo "rel='$rel'"
	echo "rev='$rev'"
	echo "verstr='$verstr'"
	;;
    -version)
	echo "$ver"
	;;
    -revision)
	echo "$rev"
	;;
    -release)
	echo "$rel"
	;;
    -print)
	echo "$verstr";
	;;
    *)
	echo "unknown argument";
	exit 1;
esac
exit 0
