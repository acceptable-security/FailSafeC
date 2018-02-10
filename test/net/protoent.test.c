/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file net/protoent.test.c
 */

#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <string.h>
#include <strings.h>
#include "common.h"

static struct protoent *dup_protoent(struct protoent *ppent)
{
  struct protoent *ppent_new;
  int n_aliases = 0;
  int i = 0;
  ppent_new = (struct protoent *)malloc(sizeof(struct protoent));
  ppent_new->p_name = (char *)malloc(sizeof(char)*(strlen(ppent->p_name) + 1));
  strcpy(ppent_new->p_name, ppent->p_name);
  while (ppent->p_aliases[n_aliases]) {
    ++n_aliases;
  }
  ppent_new->p_aliases = (char **)malloc(sizeof(char *) * (n_aliases + 1));
  for (i = 0; i < n_aliases; ++i) {
    ppent_new->p_aliases[i] = (char *)malloc(sizeof(char)*(strlen(ppent->p_aliases[i]) + 1));
    strcpy(ppent_new->p_aliases[i], ppent->p_aliases[i]);
  }
  ppent_new->p_aliases[n_aliases] = (char *)0;
  ppent_new->p_proto = ppent->p_proto;
  return ppent_new;
}

static void free_protoent(struct protoent *ppent)
{
  int i;
  free(ppent->p_name);
  for (i = 0; ppent->p_aliases[i]; ++i) {
    free(ppent->p_aliases[i]);
  }
  free(ppent->p_aliases);
}

static int compare_protoent(struct protoent *ppent1, struct protoent *ppent2)
{
  int i;
  if (strcmp(ppent1->p_name, ppent2->p_name)) {
    return 0;
  }
  for (i = 0; ppent1->p_aliases[i] && ppent2->p_aliases[i]; ++i) {
    if (strcmp(ppent1->p_aliases[i], ppent2->p_aliases[i])) {
      return 0;
    }
  }
  if (ppent1->p_aliases[i] || ppent2->p_aliases[i]) {
    return 0;
  }
  return ppent1->p_proto == ppent2->p_proto;
}

/**
 * @testname protoent_1
 * @testfor setprotoent
 * @testfor getprotoent
 * @testfor endprotoent
 * @testfor getprotobyname
 */
TEST_CASE(protoent_1)
{
  struct protoent *ppents[1024], *ppent2;
  int i = 0;
  setprotoent(0);
  while (ppents[i] = getprotoent()) {
    TEST_FAIL_IF(ppents[i]->p_name == NULL);
    TEST_FAIL_IF(ppents[i]->p_aliases == NULL);
    ppents[i] = dup_protoent(ppents[i]);
    ++i;
  }
  for (--i; i >= 0; --i) {
    ppent2 = getprotobyname(ppents[i]->p_name);
    TEST_FAIL_IF(ppent2 == NULL);
    TEST_FAIL_IF(!compare_protoent(ppents[i], ppent2));
    free_protoent(ppents[i]);
  }
  endprotoent();
  TEST(1);
}

/**
 * @testname protoent_2
 * @testfor setprotoent
 * @testfor getprotoent
 * @testfor endprotoent
 * @testfor getprotobynumber
 */
TEST_CASE(protoent_2)
{
  struct protoent *ppents[1024], *ppent2;
  int i = 0;
  setprotoent(1);
  while (ppents[i] = getprotoent()) {
    TEST_FAIL_IF(ppents[i]->p_name == NULL);
    TEST_FAIL_IF(ppents[i]->p_aliases == NULL);
    ppents[i] = dup_protoent(ppents[i]);
    ++i;
  }
  for (--i; i >= 0; --i) {
    ppent2 = getprotobynumber(ppents[i]->p_proto);
    TEST_FAIL_IF(ppent2 == NULL);
    TEST_FAIL_IF(!compare_protoent(ppents[i], ppent2));
    free_protoent(ppents[i]);
  }
  endprotoent();
  TEST(1);
}

/**
 * @testname protoent_3
 * @testfor setprotoent
 * @testfor getprotoent
 * @testfor endprotoent
 */
TEST_CASE(protoent_3)
{
  struct protoent *ppents[1024], *ppent2;
  int i = 0, j = 0;;
  setprotoent(1);
  while (ppents[i] = getprotoent()) {
    TEST_FAIL_IF(ppents[i]->p_name == NULL);
    TEST_FAIL_IF(ppents[i]->p_aliases == NULL);
    ppents[i] = dup_protoent(ppents[i]);
    ++i;
  }
  setprotoent(1);
  for (j = 0; j < i && (ppent2 = getprotoent()); ++j) {
    TEST_FAIL_IF(!compare_protoent(ppents[j], ppent2));
    free_protoent(ppents[j]);
  }
  endprotoent();
  TEST(i == j);
}

/**
 * @testname getprotobynumber_1
 * @testfor getprotobynumber
 */
TEST_CASE(getprotobynumber_1)
{
  TEST(getprotobynumber(-1) == NULL);
}

/**
 * @testname getprotobynumber_2
 * @testfor getprotobynumber
 */
TEST_CASE(getprotobynumber_2)
{
  struct protoent *ppent;
  int i = 0;

  ppent = getprotobynumber(6);
  TEST_FAIL_IF(ppent == NULL);
  TEST_FAIL_IF(strcasecmp(ppent->p_name, "tcp"));
  TEST_FAIL_IF(ppent->p_aliases == NULL);
  TEST_FAIL_IF(ppent->p_proto != 6);

  while (ppent->p_aliases[i]) {
    TEST_FAIL_IF(strcasecmp(ppent->p_aliases[i], "tcp"));
    ++i;
  }

  TEST(1);
}

/**
 * @testname getprotobynumber_3
 * @testfor getprotobynumber
 */
TEST_CASE(getprotobynumber_3)
{
  struct protoent *ppent;
  int i = 0;

  ppent = getprotobynumber(17);
  TEST_FAIL_IF(ppent == NULL);
  TEST_FAIL_IF(strcasecmp(ppent->p_name, "udp"));
  TEST_FAIL_IF(ppent->p_aliases == NULL);
  TEST_FAIL_IF(ppent->p_proto != 17);

  while (ppent->p_aliases[i]) {
    TEST_FAIL_IF(strcasecmp(ppent->p_aliases[i], "udp"));
    ++i;
  }

  TEST(1);
}
