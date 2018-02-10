/*
   This file is written by Lepidum Co., Ltd.
   Copyright (c) 2005-2006 by Lepidum Co., Ltd.
 */

int cmp_float(const void *x, const void *y)
{
  float a = *(const float *)x, b = *(const float *)y;
  if (a == b) return 0;
  else if (a < b) return -1;
  else return 1;
}
