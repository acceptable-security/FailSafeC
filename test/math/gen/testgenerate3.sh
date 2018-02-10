#!/bin/sh
# ./mathtestgen_mgr.sh <type> <function> <args_per_call> <argument>...

FLT_BOUNDARIES='0.0f -0.0f 1.0 -1.0 FLT_MIN -FLT_MIN FLT_MAX -FLT_MAX 1.0f/0.0f -1.0f/0.0f nanf("") -nanf("")'
FLT_X_PI='(float)(0.5*M_PI) (float)M_PI (float)(1.5*M_PI) (float)(2.0*M_PI) (float)(-0.5*M_PI) (float)(-1.0*M_PI) (float)(-1.5*M_PI) (float)(-2.0*M_PI) (float)(0.25*M_PI) (float)(-0.25*M_PI)'
FLT_AROUND_1='0.9f 1.1f -0.9f -1.1f'

FLT_ARGS=""
for arg1 in $FLT_BOUNDARIES $FLT_AROUND_1
do
  for arg2 in $FLT_BOUNDARIES $FLT_AROUND_1
  do
    for arg3 in $FLT_BOUNDARIES $FLT_AROUND_1
    do
      FLT_ARGS=$FLT_ARGS" ""$arg1"" ""$arg2"" ""$arg3"
    done
  done
done


DBL_BOUNDARIES='0.0 -0.0 1.0 -1.0 DBL_MIN -DBL_MIN DBL_MAX -DBL_MAX 1.0/0.0 -1.0/0.0 nan("") -nan("")'
DBL_X_PI='0.5*M_PI M_PI 1.5*M_PI 2.0*M_PI -0.5*M_PI -1.0*M_PI -1.5*M_PI -2.0*M_PI 0.25*M_PI  -0.25*M_PI'
DBL_AROUND_1='0.9 1.1 -0.9 -1.1'

DBL_ARGS=""
for arg1 in $DBL_BOUNDARIES $DBL_AROUND_1
do
  for arg2 in $DBL_BOUNDARIES $DBL_AROUND_1
  do
    for arg3 in $DBL_BOUNDARIES $DBL_AROUND_1
	do
      DBL_ARGS=$DBL_ARGS" ""$arg1"" ""$arg2"" ""$arg3"
	done
  done
done


./mathtestgen_mgr.pl fma  3 double double double 1 double $DBL_ARGS
./mathtestgen_mgr.pl fmaf 3 float float float 1 float $FLT_ARGS
