/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

struct s1 {
  short n;
};

struct s2 {
  struct s1 s;
};

struct s3 {
  int n;
  struct s2 s;
};

int main(int argc, char **argv)
{
  struct s3 s;
  return 0;
}
