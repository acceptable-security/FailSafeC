#include <stdlib.h>
#include <string.h>
#include <nl_types.h>

#include "common.h"

#define CAT "/tmp/fsc_test.cat"

static int create_catalog()
{
  int r = system("gencat --new -o " CAT " " TEST_ROOT_DIR "/msgcat/test.msg");
  return r == 0;
}

static void remove_catalog()
{
  remove(CAT);
}

static int test_catgets(nl_catd cat, int s, int m, char *def, char *t)
{
  return strcmp(t, catgets(cat, s, m, def)) == 0;
}

/**
 * @testname catgets_1
 * @testfor catopen
 * @testfor catclose
 * @testfor catgets
 */
TEST_CASE(catgets_1)
{
  nl_catd cat;
  create_catalog();
  cat = catopen(CAT, 0);
  TEST_FAIL_IF(cat == (nl_catd)-1);
  TEST_FAIL_IF(!test_catgets(cat, 1, 1, "X", "hello"));
  TEST_FAIL_IF(!test_catgets(cat, 1, 2, NULL, "world"));
  TEST_FAIL_IF(!test_catgets(cat, 2, 1, "X", "foo"));
  TEST_FAIL_IF(!test_catgets(cat, 2, 2, NULL, "bar"));
  TEST_FAIL_IF(!test_catgets(cat, 2, 4, "X", "baz"));
  TEST_FAIL_IF(!test_catgets(cat, 3, 1, "X", "X"));
  TEST_FAIL_IF(!test_catgets((nl_catd)-1, 1, 1, "X", "X"));
  TEST_FAIL_IF(catclose(cat) != 0);
  remove_catalog();
  TEST(1);
}


