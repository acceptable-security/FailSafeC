#define TEST_FUNC cosh

#define ARG_TYPE DOUBLE
#define RET_TYPE DOUBLE

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
  { 0.5*M_PI }, \
  { M_PI }, \
  { 1.5*M_PI }, \
  { 2.0*M_PI }, \
  { -0.5*M_PI }, \
  { -1.0*M_PI }, \
  { -1.5*M_PI }, \
  { -2.0*M_PI }, \
  { 0.25*M_PI }, \
  { -0.25*M_PI }, \
}
