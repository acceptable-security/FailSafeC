#include "common.h"
#include <stdlib.h>

char *mymalloc(size_t s) {
  return (char*)malloc(s);
}


TEST_CASE(fsccbug_L113)
{
  char **ptr;
  char *s = "foo";

  ptr = (char**)mymalloc(sizeof(char*));
  *ptr = s;

  ptr = (char**)realloc(ptr, 2 * sizeof(char*));

  TEST(s == *ptr);
}
