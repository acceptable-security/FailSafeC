#define TEST_FUNC sinhf

#define ARG_TYPE FLOAT
#define RET_TYPE FLOAT

#define ARGS { \
  { 0.0f }, \
  { -0.0f }, \
  { 1.0 }, \
  { -1.0 }, \
  { FLT_MIN }, \
  { -FLT_MIN }, \
  { FLT_MAX }, \
  { -FLT_MAX }, \
  { 1.0f/0.0f }, \
  { -1.0f/0.0f }, \
  { nanf("") }, \
  { -nanf("") }, \
  { (float)(0.5*M_PI) }, \
  { (float)M_PI }, \
  { (float)(1.5*M_PI) }, \
  { (float)(2.0*M_PI) }, \
  { (float)(-0.5*M_PI) }, \
  { (float)(-1.0*M_PI) }, \
  { (float)(-1.5*M_PI) }, \
  { (float)(-2.0*M_PI) }, \
  { (float)(0.25*M_PI) }, \
  { (float)(-0.25*M_PI) }, \
}
