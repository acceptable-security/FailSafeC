#!/bin/bash -e

SELFDIR=`dirname $0`

NO_REDIRECT=false

while true
do
  case "$1" in
      '-n')
	  NO_REDIRECT=true
	  ;;
      *)
	  break
  esac
  shift
done

run_test () {
  grep -v "^Ok " compile.$SUFFIX.log
  cat compile.$SUFFIX.err.log 1>&2
  ./driver.$SUFFIX
}

if $NO_REDIRECT; then
    run_test 2>&1 1>/dev/null
else
    run_test 2>test.$SUFFIX.err.log | perl $SELFDIR/common/count-failures.pl $SELFDIR/common/known-failures.txt | tee test.$SUFFIX.log
fi
