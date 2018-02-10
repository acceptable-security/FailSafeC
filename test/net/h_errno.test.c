#include "common.h"
#include <stdio.h>
#include <netdb.h>

TEST_CASE(h_errno_1)
{
  struct hostent *ent;

  h_errno = 0;
  ent = gethostbyname("www.example.jp");

  TEST_FAIL_IF(ent != NULL);

  TEST(h_errno == HOST_NOT_FOUND);
}

TEST_CASE(h_errno_store)
{
  int x;
  h_errno = (int)&x;

  TEST((int*)h_errno == &x);
}

