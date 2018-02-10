#!/bin/sh

DIR=/tmp/failsafe_c_glob_test_dir
DIR2=/tmp/failsafe_c_glob_test_dir2
DIR3=/tmp/failsafe_c_glob_test_dir3

if [ -x $DIR3 ]; then chmod -Rf 755 $DIR3; fi
rm -rf $DIR $DIR2 $DIR3
