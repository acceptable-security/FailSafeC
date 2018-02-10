#!/bin/bash -e

SELFDIR=`dirname $0`

DEBUG=
NO_REDIRECT=false

while true
do
  case "$1" in
      '-g')
	  DEBUG='-g'
	  ;;
      '-n')
	  NO_REDIRECT=true
	  ;;
      *)
	  break
  esac
  shift
done

if [ $# -gt 0 ]; then
  TESTDIR="$@"
fi

do_compile () {
rm -f testlist.c
rm -f `find $TESTDIR -type f -name '*.test.o'`

OBJFILES=''

for dir in $TESTDIR; do

  TESTFILES=`(cd $SELFDIR && find $dir -type f -name '*.test.c') | sort`

  for file in $TESTFILES; do
    echo -n "Compile $file: "
    echo -n "Compile $file: " 1>&2

    SRC=$SELFDIR/$file

    case "$file" in
    *.link.test.c)
      O=`echo $file | sed 's/\.c$//'`
      CF=''
      ;;
    *)
      O=`echo $file | sed 's/c$/o/'`
      CF='-c'
      ;;
    esac
    mkdir -p `dirname $O`

    OK='OK'
    if $FSCC $CFLAGS -I. $CF -o $O $SRC; then
      sed -r -n -e 's/^TEST_CASE(_S)?\([^)]*\)/&/p' $SRC >>testlist.c
      case "$O" in *.fail.compile.test.o) OK='Failed' ;; esac
    else
      echo '#ifdef USE_TEST_NOT_COMPILED' >>testlist.c
      sed -r -n -e 's/^TEST_CASE(_S)?\([^)]*\)/&/p' $SRC >>testlist.c
      echo '#endif' >>testlist.c
      rm -f $O
      OK='Failed'
      case "$O" in *.fail.compile.test.o) OK='OK' ;; esac
    fi
    case "$O" in
        *.compile.test.o) rm -f $O ;;
        *.link.test.o) rm -f $O ;;
        *.test.o) test "$OK" == "OK" && OBJFILES="$OBJFILES $O";;
    esac
    echo $OK
    echo $OK 1>&2
  done ;
done

rm -f test.$SUFFIX driver.$SUFFIX
echo -n "Linking tests: "
echo -n "Linking tests: " 1>&2
if $FSCC -I. $CFLAGS $LFLAGS -o test.$SUFFIX $SELFDIR/common/stub.c $OBJFILES $LIBS
then
  echo OK
  echo OK 1>&2

  echo -n "Generating test driver: "
  echo -n "Generating test driver: " 1>&2
  if $DRIVERCC -I. $DRIVERCFLAGS -o driver.$SUFFIX $SELFDIR/common/driver.c -lutil
  then
    echo OK
    echo OK 1>&2
  else
  echo "Failed! Check the log file."
  echo "Failed! Check the log file." 1>&2
  fi
else
  echo "Failed! Check the log file."
  echo "Command line was: $FSCC -I. $CFLAGS $LFLAGS -o test.$SUFFIX $SELFDIR/common/stub.c $OBJFILES $LIBS"
  echo "Failed! Check the log file." 1>&2
fi
}

if $NO_REDIRECT; then
    do_compile 2>&1 1>/dev/null
else
    do_compile 2>compile.$SUFFIX.err.log | perl $SELFDIR/common/count-failures.pl $SELFDIR/common/known-failures.txt | tee compile.$SUFFIX.log
fi
