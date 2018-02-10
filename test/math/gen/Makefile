tests = \
	acos      acosh  asin       asinh atan   atanh  cbrt   ceil   \
	cos       cosh   erfc       erf   exp2   exp    expm1  fabs   \
	floor     j0     j1         log10 log1p  log2   logb   log    \
	nearbyint rint   round      sin   sinh   sqrt   tan    tanh   \
	tgamma    trunc  y0         y1    acosf  acoshf asinf  asinhf \
	atanf     atanhf cbrtf      ceilf cosf   coshf  erfcf  erff   \
	exp2f     expf   expm1f     fabsf floorf log10f log1pf log2f  \
	logbf     logf   nearbyintf rintf roundf sinf   sinhf  sqrtf  \
	tanf      tanhf  tgammaf    truncf \
	frexp  frexpf  ilogb   ilogbf   lrint lrintf llrint llrintf \
	lround lroundf llround llroundf modf  modff  lgamma lgammaf \
	atan2  copysign   fdim    fmax     fmin    fmod  hypot  nextafter  \
	atan2f copysignf  fdimf   fmaxf    fminf   fmodf hypotf nextafterf \
	pow    remainder  remquo  scalbln  scalbn  \
	powf   remainderf remquof scalblnf scalbnf \
	scalb  jn yn \
	fma fmaf
ec = ../../../tools/embedded-c/ec

all: $(addprefix ../,$(addsuffix .test.c,$(tests)))

../%.test.c: %.h skel.ec
	($(ec) $(ECFLAGS) skel.ec -DTEST_FUNC_FILE=$< > $@ || (rm -f $@ ; false))
