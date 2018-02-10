#define TEST_FUNC llrint

#define ARG_TYPE DOUBLE
#define RET_TYPE LONG_LONG

#define ARGS { \
  { 0.0 }, \
  { -0.0 }, \
  { 1.0 }, \
  { -1.0 }, \
  { DBL_MIN }, \
  { -DBL_MIN }, \
  { DBL_MAX }, \
  { -DBL_MAX }, \
  { 1.0/0.0 }, \
  { -1.0/0.0 }, \
  { nan("") }, \
  { -nan("") }, \
  { 0.9 }, \
  { 1.1 }, \
  { -0.9 }, \
  { -1.1 }, \
}
