/* -*- c -*- */
/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Lepidum Co. Ltd. in 2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/**
 * @file stdlib/qsort.sc
 */
#include <stdlib.h>
#include <string.h>

/**
 * @fn qsort(void *base, size_t nel, size_t width, int(*compar)(const void*, const void *))
 * @brief sort any array.
 * @param base start address.
 * @param nel number of elements to sort.
 * @param width size of each element.
 * @param compar comparator function. it should return positive integer, 0, or negative intger in case the first argument is less than, equals to, or greater than the second one.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype fsc
 *
 * @author Lepidum Co., Ltd.
 */
void qsort(void *base, size_t nel, size_t width, int (*compar)(const void *, const void *))
{
  char *first, *last, *t;

  if(nel < 2){ return; }

  first = (char*)base;
  last= first + width * (nel - 1);

  for(t = first + width; t <= last; t += width){
    int d = compar(first, t);
    if(d < 0){
      goto threshold_determined;
    }
    if(d > 0){
      t = first;
      goto threshold_determined;
    }
  }
  return; /* all of elements are equal */

threshold_determined:
  {
    void *threshold, *tmp;
    int i = 0, j = nel - 1;
    char *left = first, *right = last;

    threshold = malloc(width);
    memcpy(threshold, t, width);

    tmp = malloc(width);

    while(i < j){
      for(; i < nel; i++){
        if(compar(left, threshold) >= 0){
          break;
        }
        left += width;
      }

      for(; j >= 0; j--){
        if(compar(right, threshold) < 0){
          break;
        }
        right -= width;
      }

      if(i < j){
        memcpy(tmp,   left,  width);
        memcpy(left,  right, width);
        memcpy(right, tmp,   width);
      }else{
        qsort(first, i,       width, compar);
        qsort(left,  nel - i, width, compar);
      }
    }
    free(threshold);
    free(tmp);
  }
}

/**
 * @fn void *bsearch(const void *key, const void *base, size_t nel, size_t width, int(*compar)(const void*, const void *))
 * @brief binary search.
 * @param key search key.
 * @param base start address.
 * @param nel number of elements to search.
 * @param width size of each element.
 * @param compar comparator function. it should return positive integer, 0, or negative intger in case the first argument is less than, equals to, or greater than the second one.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype fsc
 *
 * @author Lepidum Co., Ltd.
 */
void *bsearch(const void *key, const void *base, size_t nel, size_t width, int (*compar)(const void *, const void *))
{
  size_t first = 0;
  size_t last = nel;

  while (last - first > 0) {
    size_t mid = first + (last - first) / 2;
    const char *v = (const char *)base + mid * width;
    int diff = compar(key, v);
    if (diff == 0) {
      return (void *)v;
    } else if (diff < 0) {
      last = mid;
    } else {
      first = mid + 1;
    }
  }
  return NULL;
}

