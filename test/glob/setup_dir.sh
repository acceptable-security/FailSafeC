#!/bin/sh

DIR=/tmp/failsafe_c_glob_test_dir
DIR2=/tmp/failsafe_c_glob_test_dir2
DIR3=/tmp/failsafe_c_glob_test_dir3

mkdir $DIR
touch $DIR/foo
touch $DIR/bar
touch $DIR/baz
touch $DIR/hoge
touch $DIR/piyo

mkdir $DIR2
touch "$DIR2/\a"
touch "$DIR2/a"

mkdir $DIR3
mkdir -m 000 $DIR3/foo
mkdir -m 000 $DIR3/bar
