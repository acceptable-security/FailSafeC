#!/bin/sh
# ./mathtestgen_mgr.sh <type> <function> <args_per_call> <argument>...

FLT_BOUNDARIES='0.0f -0.0f 1.0 -1.0 FLT_MIN -FLT_MIN FLT_MAX -FLT_MAX 1.0f/0.0f -1.0f/0.0f nanf("") -nanf("")'
FLT_X_PI='(float)(0.5*M_PI) (float)M_PI (float)(1.5*M_PI) (float)(2.0*M_PI) (float)(-0.5*M_PI) (float)(-1.0*M_PI) (float)(-1.5*M_PI) (float)(-2.0*M_PI) (float)(0.25*M_PI) (float)(-0.25*M_PI)'
FLT_AROUND_1='0.9f 1.1f -0.9f -1.1f'

DBL_BOUNDARIES='0.0 -0.0 1.0 -1.0 DBL_MIN -DBL_MIN DBL_MAX -DBL_MAX 1.0/0.0 -1.0/0.0 nan("") -nan("")'
DBL_X_PI='0.5*M_PI M_PI 1.5*M_PI 2.0*M_PI -0.5*M_PI -1.0*M_PI -1.5*M_PI -2.0*M_PI 0.25*M_PI  -0.25*M_PI'
DBL_AROUND_1='0.9 1.1 -0.9 -1.1'

#./mathtestgen_mgr.sh double sin 1 $DBL_BOUNDARIES

./mathtestgen_mgr.pl acos      1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl acosh     1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl asin      1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl asinh     1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl atan      1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl atanh     1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl cbrt      1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl ceil      1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl cos       1 double 1 double $DBL_BOUNDARIES $DBL_X_PI
./mathtestgen_mgr.pl cosh      1 double 1 double $DBL_BOUNDARIES $DBL_X_PI
./mathtestgen_mgr.pl erfc      1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl erf       1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl exp2      1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl exp       1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl expm1     1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl fabs      1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl floor     1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl j0        1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl j1        1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl log10     1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl log1p     1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl log2      1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl logb      1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl log       1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl nearbyint 1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl rint      1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl round     1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl sin       1 double 1 double $DBL_BOUNDARIES $DBL_X_PI
./mathtestgen_mgr.pl sinh      1 double 1 double $DBL_BOUNDARIES $DBL_X_PI
./mathtestgen_mgr.pl sqrt      1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl tan       1 double 1 double $DBL_BOUNDARIES $DBL_X_PI
./mathtestgen_mgr.pl tanh      1 double 1 double $DBL_BOUNDARIES $DBL_X_PI
./mathtestgen_mgr.pl tgamma    1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl trunc     1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl y0        1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl y1        1 double 1 double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl acosf      1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl acoshf     1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl asinf      1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl asinhf     1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl atanf      1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl atanhf     1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl cbrtf      1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl ceilf      1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl cosf       1 float 1 float $FLT_BOUNDARIES $FLT_X_PI
./mathtestgen_mgr.pl coshf      1 float 1 float $FLT_BOUNDARIES $FLT_X_PI
./mathtestgen_mgr.pl erfcf      1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl erff       1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl exp2f      1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl expf       1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl expm1f     1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl fabsf      1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl floorf     1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl log10f     1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl log1pf     1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl log2f      1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl logbf      1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl logf       1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl nearbyintf 1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl rintf      1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl roundf     1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl sinf       1 float 1 float $FLT_BOUNDARIES $FLT_X_PI
./mathtestgen_mgr.pl sinhf      1 float 1 float $FLT_BOUNDARIES $FLT_X_PI
./mathtestgen_mgr.pl sqrtf      1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl tanf       1 float 1 float $FLT_BOUNDARIES $FLT_X_PI
./mathtestgen_mgr.pl tanhf      1 float 1 float $FLT_BOUNDARIES $FLT_X_PI
./mathtestgen_mgr.pl tgammaf    1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl truncf     1 float 1 float $FLT_BOUNDARIES $FLT_AROUND_1

./mathtestgen_mgr.pl ilogb     1 double 1 int $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl ilogbf    1 float  1 int $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl lrint     1 double 1 long $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl lrintf    1 float  1 long $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl lround    1 double 1 long $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl lroundf   1 float  1 long $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl llrint    1 double 1 'long long' $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl llrintf   1 float  1 'long long' $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl llround   1 double 1 'long long' $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl llroundf  1 float  1 'long long' $FLT_BOUNDARIES $FLT_AROUND_1

./mathtestgen_mgr.pl frexp     1 double 2 double int $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl frexpf    1 float  2 float  int $FLT_BOUNDARIES $FLT_AROUND_1
./mathtestgen_mgr.pl modf      1 double 2 double double $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl modff     1 float  2 float  float  $FLT_BOUNDARIES $FLT_AROUND_1

./mathtestgen_mgr.pl lgamma    1 double 2 double 'int(signgam)' $DBL_BOUNDARIES $DBL_AROUND_1
./mathtestgen_mgr.pl lgammaf   1 float  2 float  'int(signgam)' $FLT_BOUNDARIES $FLT_AROUND_1
