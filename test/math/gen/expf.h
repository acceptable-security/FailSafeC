#define TEST_FUNC expf

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
  { 0.9f }, \
  { 1.1f }, \
  { -0.9f }, \
  { -1.1f }, \
}
