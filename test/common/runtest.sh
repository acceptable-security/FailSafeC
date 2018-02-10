#!/bin/bash -e

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

if [ -z "$FSCC" ]; then
  FSCC='../../driver/fscc --save-temps'
fi

CC="${FSCC%% *}"
CCNAME="`basename $CC`"

case "$CCNAME" in
fscc)
  DRIVERCFLAGS="$DEBUG"
  CFLAGS="$DEBUG"
  LFLAGS='-Wl,-lcrypt -Wl,-lgdbm -Wl,-lgdbm_compat'
  LIBS='-lfsc -lndbm'
  ;;
*)
  DRIVERCFLAGS="-DFSC_ABRT=GCC_ABRT $DEBUG"
  CFLAGS="-std=c99 -trigraphs -O2 -D_POSIX_C_SOURCE=200112L -D_XOPEN_SOURCE=500 $EXTRA_CFLAGS"
  LFLAGS='-lcrypt -lm -lgdbm -lgdbm_compat'
  LIBS=''
  ;;
esac

TESTDIR='../baseline ../fscc ../stdio ../stdlib ../string ../unix ../include ../net ../math ../glob ../ndbm ../iconv ../msgcat'

if [ $# -gt 0 ]; then
  TESTDIR="$@"
fi

TESTFILES=`find $TESTDIR -type f -name '*.test.c' | sort`

do_test () {
rm -f testlist.c
rm -f `find $TESTDIR -type f -name '*.test.o'`

OBJFILES=''

for file in $TESTFILES; do
  echo -n "Compile $file: "
  echo -n "Compile $file: " 1>&2

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
  OK='OK'
  if $FSCC -I. -DTEST_ROOT_DIR='".."' $CFLAGS $CF -o $O $file; then
    sed -r -n -e 's/^TEST_CASE(_S)?\([^)]*\)/&/p' $file >>testlist.c
    case "$O" in *.fail.compile.test.o) OK='Failed' ;; esac
  else
    echo '#ifdef USE_TEST_NOT_COMPILED' >>testlist.c
    sed -r -n -e 's/^TEST_CASE(_S)?\([^)]*\)/&/p' $file >>testlist.c
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
done

rm -f test driver
echo -n "Linking tests: "
echo -n "Linking tests: " 1>&2
if $FSCC -I. $CFLAGS $LFLAGS -o test stub.c $OBJFILES $LIBS
then
  echo OK
  echo OK 1>&2

  echo -n "Generating test driver: "
  echo -n "Generating test driver: " 1>&2
  if gcc -I. $DRIVERCFLAGS -o driver driver.c -lutil
  then
    echo OK
    echo OK 1>&2
    ./driver
  else
  echo "Failed! Check the log file."
  echo "Failed! Check the log file." 1>&2
  fi
else
  echo "Failed! Check the log file."
  echo "Failed! Check the log file." 1>&2
fi
}

if $NO_REDIRECT; then
    do_test 2>&1 1>/dev/null
else
    do_test 2>test.err.log | perl count-failures.pl known-failures.txt | tee test.log
fi
