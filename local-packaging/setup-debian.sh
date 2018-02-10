#!/bin/sh -e
if [ ! -d local-packaging/debian-dir -a -f setup-debian.sh ]; then
  # wrongly running inside local-packaging dir
  cd ..
fi
if [ ! -d local-packaging/debian-dir ]; then
  echo "Run this script from the top source directory."
  exit 1
fi

if [ ! -L debian ]; then
  if [ -d debian ]; then
    echo "Sorry, you can't create local packages from already Debianized sources"
    echo "(real debian directory exists)."
    echo ""
    echo "Try to use \"official\" debian packaging methods instead"
    echo "(e.g. \"dpkg-buildpackage -rfakeroot\")."
    echo ""
    if [ ! -d debian/control ]; then
      echo "Oops, but no debian-specific files... why?"
      exit 2
    fi
    exit 1
  fi
  ln -s local-packaging/debian-dir debian
  if [ ! -d debian/. ]; then
    echo "Oops, installing local debian files failed... why?"
    exit 2
  fi 
  echo "debian directory created."
else
  echo "debian directory for local packages already exists."
fi

if [ ! -f debian/MAINTAINER ]; then
  if tty -s; then
    echo "---------------------------"
    echo "No stored maintainer information is found.  Creating it."
    echo ""
  else
    echo "---------------------------"
    echo "No stored maintainer information is found.  Aborting."
    echo "See debian/make-changelog.sh for more information."
    echo "---------------------------"
    exit 1
  fi
  while [ "$ok" != "yes" ]; do
    echo -n "Input maintainer's name: "
    read name
    echo -n "Input maintainer's mail address: "
    read address
    maintainer="$name <$address>"
    echo ""
    echo "Maintainer: \"$maintainer\""
    echo "... Is this correct? (yes/other)"
    read ok
  done
  echo "---------------------------"
  echo "$maintainer" > debian/MAINTAINER
  echo "Maintainer information saved in debian/MAINTAINER."
else
  echo "Maintainer information is already stored, using it: "
  echo -n "    "
  cat debian/MAINTAINER
fi
debian/update-changelog.sh debian/changelog
