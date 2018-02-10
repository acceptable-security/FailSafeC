#include "common.h"

static void SWAP(int *x, int *y) {
  int t;
  t = *x;
  *x = *y;
  *y = t;
}

static void qsort_int(int *p, unsigned int len) {
  int pivot;
  int i, j, mid;
  int *l, *r;
  if (len <= 1)
    return;
  if (len == 2) {
    if (p[0] > p[1]) {
      SWAP(&p[0], &p[1]);
    }
    return;
  }
  mid = len / 2;

  if (p[0] > p[mid])
    SWAP(&p[0], &p[mid]);
  if (p[mid] > p[len - 1]) {
    SWAP(&p[mid], &p[len - 1]);
    if (p[0] > p[mid])
      SWAP(&p[0], &p[mid]);
  }
  pivot = p[mid];
  l = p; r = &p[len - 1];
  do {
    while(*l < pivot)
      l++;
    while(*r > pivot)
      r--;
    if (l < r) {
      SWAP(l, r);
      l++;
      r--;
    }
    else if (l == r) {
      l++;
      r--;
      break;
    }
  } while (l <= r);

  qsort_int(p, (r - p) + 1);
  qsort_int(l, len - (l - p));
}

TEST_CASE(baseline_apptest_qsort)
{
    int a[] = { 1120, 6108, 3716, 5520, 2915, 7119, 1162, 5715, 2700,
		3302, 3837, 9535, 6718, 2025, 3445, 3015, 8867, 4978,
		8210, 1783, 8294, 2784, 8432, 1759, 8234, 8693, 4500 };
    int n = sizeof(a) / sizeof(int), i;
    qsort_int(a, n);

    TEST(a[0] == 1120);
    for(i = 1; i < n; i++)
	TEST(a[i - 1] < a[i]); /* no duplicated element in the test input */
    TEST(a[n-1] == 9535);
}
