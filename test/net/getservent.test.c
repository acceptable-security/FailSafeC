/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

/**
 * @file net/getservent.test.c
 */

#include <stdlib.h>
#include <string.h>
#include <netdb.h>
#include "common.h"

static struct servent *dup_servent(struct servent *psent)
{
  struct servent *psent_new;
  int n_aliases = 0;
  int i = 0;
  psent_new = (struct servent *)malloc(sizeof(struct servent));
  psent_new->s_name = (char *)malloc(sizeof(char)*(strlen(psent->s_name)+1));
  strcpy(psent_new->s_name, psent->s_name);
  while (psent->s_aliases[n_aliases]) {
    ++n_aliases;
  }
  psent_new->s_aliases = (char **)malloc(sizeof(char *) * (n_aliases + 1));
  for (i = 0; i < n_aliases; ++i) {
    psent_new->s_aliases[i] = (char *)malloc(sizeof(char) * (strlen(psent->s_aliases[i]) + 1));
    strcpy(psent_new->s_aliases[i], psent->s_aliases[i]);
  }
  psent_new->s_aliases[n_aliases] = NULL;
  psent_new->s_port = psent->s_port;
  psent_new->s_proto = (char *)malloc(sizeof(char)*(strlen(psent->s_proto)+1));
  strcpy(psent_new->s_proto, psent->s_proto);
  return psent_new;
}

static void free_servent(struct servent *psent)
{
  int i = 0;
  free(psent->s_name);
  for (i = 0; psent->s_aliases[i]; ++i)
  {
    free(psent->s_aliases[i]);
  }
  free(psent->s_aliases);
  free(psent->s_proto);
  free(psent);
}

static int compare_servent(struct servent *psent1, struct servent *psent2)
{
  int i = 0;
  if (strcmp(psent1->s_name, psent2->s_name)) {
    return 0;
  }
  for (i = 0; psent1->s_aliases[i] && psent2->s_aliases[i]; ++i) {
    if (strcmp(psent1->s_aliases[i], psent2->s_aliases[i])) {
      return 0;
    }
  }
  if (psent1->s_aliases[i] || psent2->s_aliases[i]) {
    return 0;
  }
  if (psent1->s_port != psent2->s_port) {
    return 0;
  }
  if (strcmp(psent1->s_proto, psent2->s_proto)) {
    return 0;
  }
  return 1;
}

/**
 * @testname servent_1
 * @testfor setservent
 * @testfor getservent
 * @testfor endservent
 */
TEST_CASE(servent_1)
{
  struct servent *psents[2048], *psent2;
  int i = 0, j = 0;
  setservent(0);
  while (psents[i] = getservent()) {
    TEST_FAIL_IF(psents[i]->s_name == NULL);
    TEST_FAIL_IF(psents[i]->s_aliases == NULL);
    TEST_FAIL_IF(psents[i]->s_proto == NULL);
    psents[i] = dup_servent(psents[i]);
    ++i;
  }
  endservent();
  for (j = 0; j < i; ++j) {
    psent2 = getservbyname(psents[j]->s_name, psents[j]->s_proto);
    TEST_FAIL_IF(psent2 == NULL);
    TEST_FAIL_IF(!compare_servent(psents[j], psent2));
    free_servent(psents[j]);
  }
  TEST(1);
}

/**
 * @testname servent_2
 * @testfor setservent
 * @testfor getservent
 * @testfor endservent
 */
TEST_CASE(servent_2)
{
  struct servent *psents[2048], *psent2;
  int i = 0, j = 0;
  setservent(1);
  while (psents[i] = getservent()) {
    TEST_FAIL_IF(psents[i]->s_name == NULL);
    TEST_FAIL_IF(psents[i]->s_aliases == NULL);
    TEST_FAIL_IF(psents[i]->s_proto == NULL);
    psents[i] = dup_servent(psents[i]);
    ++i;
  }
  setservent(1);
  for (j = 0; j < i; ++j) {
    psent2 = getservent();
    TEST_FAIL_IF(psent2 == NULL);
    TEST_FAIL_IF(!compare_servent(psents[j], psent2));
    free_servent(psents[j]);
  }
  endservent();
  TEST(1);
}
