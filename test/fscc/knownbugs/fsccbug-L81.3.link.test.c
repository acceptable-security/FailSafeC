/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

#include <netdb.h>
#include <stdio.h>

int main(int argc, char **argv){
  struct servent *ent = getservbyname("echo", "tcp");
  printf("%s\n", ent->s_name);
  return 0;
}
