#!/bin/bash
# This script generates temporary debian/changelog file
# for building local Debian package from svn or tar archives.
#
# "Maintained" packages should not use this script:
#   please edit debian/rules to disable this.
#
target=debian/changelog

if test ! -f "$target.tmpl"; then
  echo "$target.tmpl not found."
  exit 1
fi

if test -f VERSION
then
    version="`cat VERSION`"
elif test -d .svn
then
    version=`tools/disthelpers/get-version-number.sh -version`
else
    echo "cannot determine revision number."
    exit 1
fi

date="`LANG=C date +'%a, %e %b %Y %T %z'`"

if test -f debian/MAINTAINER
then
    maintainer="`cat debian/MAINTAINER`"
else
    echo "No stored maintainer information is found.  Aborting."
    echo "See debian.localpkg/README.Debian-packaging for more information."
    exit 1
fi

generate () {
  sed -e "s/%%rev%%/$version/" -e "s/%%date%%/$date/" -e "s/%%maintainer%%/$maintainer/" \
  "$target.tmpl" > "$1"
}

if [ "$1" = "-f" ]; then
  generate "$target"
elif [ ! -f "$target" ]; then
  generate "$target"
else
  oldrev="`dpkg-parsechangelog | sed -ne 's/Version: //p'`"
  if [ "$oldrev" = "$version" ]; then
    if [ "$1" = "-u" ]; then
      generate "$target.new"
      sed -e '1d; 1,/^[^ ]/ { /^ /d; /^$/d }' "$target" >> "$target.new"
      mv "$target.new" "$target"
    fi
  else
    generate "$target.new"
    cat "$target" >> "$target.new"
    mv "$target.new" "$target"
  fi
fi

