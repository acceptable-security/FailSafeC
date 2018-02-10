#!/bin/sh
# ./mathtestgen_mgr.sh <type> <function> <args_per_call> <argument>...

FLT_BOUNDARIES='0.0f -0.0f 1.0 -1.0 FLT_MIN -FLT_MIN FLT_MAX -FLT_MAX 1.0f/0.0f -1.0f/0.0f nanf("") -nanf("")'
FLT_X_PI='(float)(0.5*M_PI) (float)M_PI (float)(1.5*M_PI) (float)(2.0*M_PI) (float)(-0.5*M_PI) (float)(-1.0*M_PI) (float)(-1.5*M_PI) (float)(-2.0*M_PI) (float)(0.25*M_PI) (float)(-0.25*M_PI)'
FLT_AROUND_1='0.9f 1.1f -0.9f -1.1f'
FLT_FACTORS='-254 -127 -1 0 1 127 254'

FLT_ARGS=""
FLT_ARGS2=""
FLT_ARGS3=""
for arg1 in $FLT_BOUNDARIES $FLT_AROUND_1
do
  for arg2 in $FLT_BOUNDARIES $FLT_AROUND_1
  do
    FLT_ARGS=$FLT_ARGS" ""$arg1"" ""$arg2"
    if [ "$arg2" != 'nanf("")' -a "$arg2" != '-nanf("")' ] ; then 
      FLT_ARGS2=$FLT_ARGS2" ""$arg1"" ""$arg2"
    fi
  done
  for arg2 in $FLT_FACTORS
  do
    FLT_ARGS3=$FLT_ARGS3" ""$arg1"" ""$arg2"
  done
done



DBL_BOUNDARIES='0.0 -0.0 1.0 -1.0 DBL_MIN -DBL_MIN DBL_MAX -DBL_MAX 1.0/0.0 -1.0/0.0 nan("") -nan("")'
DBL_X_PI='0.5*M_PI M_PI 1.5*M_PI 2.0*M_PI -0.5*M_PI -1.0*M_PI -1.5*M_PI -2.0*M_PI 0.25*M_PI  -0.25*M_PI'
DBL_AROUND_1='0.9 1.1 -0.9 -1.1'
DBL_FACTORS='-2046 -1027 -1 0 1 1027 2046'

DBL_ARGS=""
DBL_ARGS2=""
for arg1 in $DBL_BOUNDARIES $DBL_AROUND_1
do
  for arg2 in $DBL_BOUNDARIES $DBL_AROUND_1
  do
    DBL_ARGS=$DBL_ARGS" ""$arg1"" ""$arg2"
    if [ "$arg2" != 'nan("")' -a "$arg2" != '-nan("")' ] ; then 
      DBL_ARGS2=$DBL_ARGS2" ""$arg1"" ""$arg2"
    fi
  done
  for arg2 in $DBL_FACTORS
  do
    DBL_ARGS3=$DBL_ARGS3" ""$arg1"" ""$arg2"
  done
  for arg0 in 0 1 2 -1
  do
    DBL_JN_ARGS=$DBL_JN_ARGS" ""$arg0"" ""$arg1"
  done
done


./mathtestgen_mgr.pl atan2     2 double double 1 double $DBL_ARGS
./mathtestgen_mgr.pl copysign  2 double double 1 double $DBL_ARGS2
./mathtestgen_mgr.pl fdim      2 double double 1 double $DBL_ARGS
./mathtestgen_mgr.pl fmax      2 double double 1 double $DBL_ARGS
./mathtestgen_mgr.pl fmin      2 double double 1 double $DBL_ARGS
./mathtestgen_mgr.pl fmod      2 double double 1 double $DBL_ARGS
./mathtestgen_mgr.pl hypot     2 double double 1 double $DBL_ARGS
./mathtestgen_mgr.pl nextafter 2 double double 1 double $DBL_ARGS
./mathtestgen_mgr.pl pow       2 double double 1 double $DBL_ARGS
./mathtestgen_mgr.pl remainder 2 double double 1 double $DBL_ARGS
./mathtestgen_mgr.pl atan2f     2 float float 1 float $FLT_ARGS 
./mathtestgen_mgr.pl copysignf  2 float float 1 float $FLT_ARGS2
./mathtestgen_mgr.pl fdimf      2 float float 1 float $FLT_ARGS
./mathtestgen_mgr.pl fmaxf      2 float float 1 float $FLT_ARGS
./mathtestgen_mgr.pl fminf      2 float float 1 float $FLT_ARGS
./mathtestgen_mgr.pl fmodf      2 float float 1 float $FLT_ARGS
./mathtestgen_mgr.pl hypotf     2 float float 1 float $FLT_ARGS
./mathtestgen_mgr.pl nextafterf 2 float float 1 float $FLT_ARGS
./mathtestgen_mgr.pl powf       2 float float 1 float $FLT_ARGS
./mathtestgen_mgr.pl remainderf 2 float float 1 float $FLT_ARGS

./mathtestgen_mgr.pl remquo    2 double double 2 double int $DBL_ARGS
./mathtestgen_mgr.pl remquof   2 float  float  2 float  int $FLT_ARGS

./mathtestgen_mgr.pl scalbln   2 double long   1 double $DBL_ARGS3
./mathtestgen_mgr.pl scalbn    2 double int    1 double $DBL_ARGS3
./mathtestgen_mgr.pl ldexp     2 double int    1 double $DBL_ARGS3
./mathtestgen_mgr.pl scalblnf  2 float  long   1 float  $FLT_ARGS3
./mathtestgen_mgr.pl scalbnf   2 float  int    1 float  $FLT_ARGS3
./mathtestgen_mgr.pl ldexpf    2 flaot  int    1 float  $FLT_ARGS3

./mathtestgen_mgr.pl scalb     2 double double 1 double $DBL_ARGS
./mathtestgen_mgr.pl jn        2 int    double 1 double $DBL_JN_ARGS
./mathtestgen_mgr.pl yn        2 int    double 1 double $DBL_JN_ARGS
