/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file net/getnetent.test.c
 */

#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include <string.h>
#include "common.h"

static struct netent *dup_netent(struct netent *pnent)
{
  struct netent *pnent_new;
  int n_aliases = 0;
  int i = 0;
  pnent_new = (struct netent *)malloc(sizeof(struct netent));
  pnent_new->n_name = (char *)malloc(sizeof(char)*(strlen(pnent->n_name) + 1));
  strcpy(pnent_new->n_name, pnent->n_name);
  while (pnent->n_aliases[n_aliases]) {
    ++n_aliases;
  }
  pnent_new->n_aliases = (char **)malloc(sizeof(char *) * (n_aliases + 1));
  for (i = 0; i < n_aliases; ++i) {
    pnent_new->n_aliases[i] = (char *)malloc(sizeof(char)*(strlen(pnent->n_aliases[i]) + 1));
    strcpy(pnent_new->n_aliases[i], pnent->n_aliases[i]);
  }
  pnent_new->n_aliases[n_aliases] = NULL;
  pnent_new->n_addrtype = pnent->n_addrtype;
  pnent_new->n_net = pnent->n_net;
  return pnent_new;
}

static void free_netent(struct netent *pnent)
{
  int i;
  free(pnent->n_name);
  for (i = 0; pnent->n_aliases[i]; ++i) {
    free(pnent->n_aliases[i]);
  }
  free(pnent->n_aliases);
  free(pnent);
}

static int compare_netent(struct netent *pnent1, struct netent *pnent2)
{
  int i;
  if (strcmp(pnent1->n_name, pnent2->n_name)) {
    return 0;
  }
  for (i = 0; pnent1->n_aliases[i] && pnent2->n_aliases[i]; ++i) {
    if (strcmp(pnent1->n_aliases[i], pnent2->n_aliases[i])) {
      return 0;
    }
  }
  if (pnent1->n_aliases[i] || pnent2->n_aliases[i]) {
    return 0;
  }
  if (pnent1->n_addrtype != pnent2->n_addrtype) {
    return 0;
  }
  if (pnent1->n_net != pnent2->n_net) {
    return 0;
  }
  return 1;
}

/**
 * @testname netent_1
 * @testfor setnetent
 * @testfor getnetent
 * @testfor endnetent
 */
TEST_CASE(netent_1)
{
  struct netent *pnents[1024], *pnent2;
  int i = 0, j = 0;
  sethostent(0);
  while (pnents[i] = getnetent()) {
    TEST_FAIL_IF(pnents[i]->n_name == NULL);
    TEST_FAIL_IF(pnents[i]->n_aliases == NULL);
    TEST_FAIL_IF(pnents[i]->n_addrtype != AF_INET);
    pnents[i] = dup_netent(pnents[i]);
    ++i;
  }
  endhostent();
  for (j = 0; j < i; ++j) {
    pnent2 = getnetbyname(pnents[j]->n_name);
    TEST_FAIL_IF(pnent2 == NULL);
    TEST_FAIL_IF(!compare_netent(pnents[j], pnent2));
    free_netent(pnents[j]);
  }
  TEST(1);
}

/**
 * @testname netent_2
 * @testfor setnetent
 * @testfor getnetent
 * @testfor endhostent
 */
TEST_CASE(netent_2)
{
  struct netent *pnents[1024], *pnent2;
  int i = 0, j = 0;
  setnetent(1);
  while (pnents[i] = getnetent()) {
    TEST_FAIL_IF(pnents[i]->n_name == NULL);
    TEST_FAIL_IF(pnents[i]->n_aliases == NULL);
    TEST_FAIL_IF(pnents[i]->n_addrtype != AF_INET);
    pnents[i] = dup_netent(pnents[i]);
    ++i;
  }
  setnetent(1);
  for (j = 0; j < i && (pnent2 = getnetent()); ++j) {
    TEST_FAIL_IF(!compare_netent(pnents[j], pnent2));
    free_netent(pnents[j]);
  }
  TEST(i == j);
}

/**
 * @testname netent_3
 * @testfor getnetbyaddr
 * @testfor getnetbyname
 */
TEST_CASE(netent_3)
{
  struct netent *pnents[1024], *pnent2;
  int i = 0, j = 0;
  setnetent(1);
  while (pnents[i] = getnetent()) {
    TEST_FAIL_IF(pnents[i]->n_name == NULL);
    TEST_FAIL_IF(pnents[i]->n_aliases == NULL);
    TEST_FAIL_IF(pnents[i]->n_addrtype != AF_INET);
    pnents[i] = dup_netent(pnents[i]);
    ++i;
  }
  endnetent();
  for (j = 0; j < i; ++j) {
    pnent2 = getnetbyaddr(pnents[j]->n_net, pnents[j]->n_addrtype);
    TEST_FAIL_IF(pnent2 == NULL);
    TEST_FAIL_IF(!compare_netent(pnents[j], pnent2));
    pnent2 = getnetbyname(pnents[j]->n_name);
    TEST_FAIL_IF(pnent2 == NULL);
    TEST_FAIL_IF(!compare_netent(pnents[j], pnent2));
    free_netent(pnents[j]);
  }
  TEST(1);
}
