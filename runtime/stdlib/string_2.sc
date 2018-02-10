/* -*- c -*- */
/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Yutaka Oiwa and Lepidum Co. Ltd. in 2004-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/**
 * @file stdlib/string_2.sc
 */

#include <sys/types.h>
#include <ctype.h>
#include <stdlib.h>
#include <limits.h>
#include <string.h>

/**
 * @fn char *strcpy(char *dest, const char *src)
 * @author Yutaka Oiwa.
 */
char *strcpy(char *dest, const char *src) {
    char *d = dest;
    while(*d++ = *src++) ;
    return dest;
}

/**
 * @fn char *strncpy(char *dest, const char *src, size_t n)
 * @brief copy string up to count.
 * @param dest destination array.
 * @param src string to copy.
 * @param n maximum length to copy
 * @return parameter dest is returned.
 *
 * @crashcase illegal pointer (dest, src), buffer overflow.
 * @fsctype string(rw)
 *
 * @author Lepidum Co., Ltd.
 */
char *strncpy(char *dest, const char *src, size_t n)
{
  char *d = dest;
  unsigned long i;
  for(i = 0; i < n; i++){
    if((*d++ = *src++) == '\0'){
      for(i++; i < n; i++){
        *d++ = '\0';
      }
      break;
    }
  }
  return dest;
}

/**
 * @fn char *strcat(char *dest, const char *src)
 * @author Yutaka Oiwa.
 */
char *strcat(char *dest, const char *src) {
    char *d = dest;
    while (*d) d++;
    while(*d++ = *src++) ;
    return dest;
}

/**
 * @fn char *strncat(char *dest, const char *src, size_t n)
 * @brief concatenate part of strings.
 * @param dest the first string argument and result buffer.
 * @param src the second string argument.
 * @param n maximum length to copy
 * @return dest is returned.
 *
 * @crashcase illegal pointer, buffer overflow, non-terminated string
 * @fsctype string(rw)
 *
 * @author Lepidum Co., Ltd.
 */
char *strncat(char *dest, const char *src, size_t n) {
  size_t i;
  char *d = dest;
  while (*d) d++;
  for (i = 0; i < n; i++) {
    if ((*d++ = *src++) == '\0') {
      return dest;
    }
  }
  *d = '\0';
  return dest;
}

/**
 * @fn int strncmp(const char *s1t, const char *s2, size_t n)
 * @author Yutaka Oiwa.
 */
int strncmp(const char *s1, const char *s2, size_t n) {
    int i;
    for (i = 0; i < n; i++) {
	unsigned char c1 = *s1++, c2 = *s2++;
	if (c1 < c2)
	    return -1;
	else if (c1 > c2)
	    return 1;
	else if (c1 == 0)
	    return 0;
    }
    return 0;
}

/**
 * @fn int strcmp(const char *s1t, const char *s2)
 * @author Yutaka Oiwa.
 */
int strcmp(const char *s1, const char *s2) {
    int i;
    for(;;) {
	unsigned char c1 = *s1++, c2 = *s2++;
	if (c1 < c2)
	    return -1;
	else if (c1 > c2)
	    return 1;
	else if (c1 == 0)
	    return 0;
    }
}

/**
 * @fn int strncasecmp(const char *s1, const char *s2, size_t n)
 * @brief compare two strings with ignoring case.
 * @param s1 first string.
 * @param s2 second string.
 * @param n count to compare.
 * @retval >0 s1 is greater than s2.
 * @retval 0  s1 is equal to s2.
 * @retval <0 s1 is less than s2.
 *
 * @crashcase illegal pointer, non-terminated string, buffer overflow
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
int strncasecmp(const char *s1, const char *s2, size_t n)
{
  int i;
  for (i = 0; i < n; i++) {
    unsigned char c1 = *s1++, c2 = *s2++;
    unsigned char c3 = (unsigned char)tolower(c1), c4 = (unsigned char)tolower(c2);
    if (c3 < c4)
      return -1;
    else if (c3 > c4)
      return 1;
    else if (c3 == 0)
      return 0;
  }
  return 0;
}

/**
 * @fn int strcasecmp(const char *s1, const char *s2)
 * @brief compare two strings with ignoring case.
 * @param s1 first string.
 * @param s2 second string.
 * @retval >0 s1 is greater than s2.
 * @retval 0  s1 is equal to s2.
 * @retval <0 s1 is less than s2.
 *
 * @crashcase illegal pointer, non-terminated string
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
int strcasecmp(const char *s1, const char *s2) {
  for (;;) {
    unsigned char c1 = *s1++, c2 = *s2++;
    unsigned char c3 = (unsigned char)tolower(c1), c4 = (unsigned char)tolower(c2);
    if (c3 < c4)
      return -1;
    else if (c3 > c4)
      return 1;
    else if (c3 == 0)
      return 0;
  }
}

/**
 * @fn int memcmp(const void *s1, const void *s2, size_t n)
 * @brief compare two memory blocks.
 * @param s1 memory block.
 * @param s2 memory block.
 * @param n size of block to compare.
 * @retval >0 s1 is greater than s2.
 * @retval 0 s1 is equal to s2.
 * @retval <0 s1 is less than s2.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype memory(ro)
 *
 * @author Lepidum Co., Ltd.
 */
int memcmp(const void *s1, const void *s2, size_t n){
  const unsigned char *p1 = s1, *p2 = s2;
  size_t i;

  for(i = 0; i < n; i++){
    unsigned char c1 = *p1++, c2 = *p2++;

    if(c1 > c2){ return 1; }
    if(c1 < c2){ return -1; }
  }
  return 0;
}

/**
 * @fn char *strchr(const char * s, int t)
 * @author Yutaka Oiwa.
 */
char *strchr(const char *s, int t)
{
  char c;
  for(;; s++){
    c = *s;
    if(c == (char)t){ return (char*)s; }
    if(!c){ return 0; }
  }
}

/**
 * @fn char *strrchr(const char * s, int t)
 * @author Yutaka Oiwa.
 */
char *strrchr(const char *s, int t)
{
  char c;
  char *p = 0;
  for(;; s++){
    c = *s;
    if(c == (char)t){ p = (char*)s; }
    if(!c){ return p; }
  }
}

/**
 * @fn char *index(const char * s, int t)
 * @author Yutaka Oiwa.
 */
char *index(const char *s, int t)
{
  char c;
  for(;; s++){
    c = *s;
    if(c == (char)t){ return (char*)s; }
    if(!c){ return 0; }
  }
}

/**
 * @fn char *rindex(const char * s, int t)
 * @author Yutaka Oiwa.
 */
char *rindex(const char *s, int t)
{
  char c;
  char *p = 0;
  for(;; s++){
    c = *s;
    if(c == (char)t){ p = (char*)s; }
    if(!c){ return p; }
  }
}

/**
 * @fn void *memchr(const void *s, int c, size_t n)
 * @brief search byte in memory block.
 * @param s start address
 * @param c search target byte
 * @param n search length
 * @retval !=NULL pointer to first c found returned.
 * @retval NULL c not found.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype atomic, pointer(atomic, ro)
 *
 * @author Lepidum Co., Ltd.
 */
void *memchr(const void *s, int c, size_t n){
  unsigned char *p = (unsigned char*)s;
  unsigned long i;
  for(i = 0; i < n; i++){
    if(*p == (unsigned char)c){
      return (void*)p;
    }
    p++;
  }
  return 0;
}

/**
 * @fn void *memccpy(void *dst, const void *src, int c, size_t n)
 * @brief copy memory
 *
 * @author Lepidum Co., Ltd.
 */
void *memccpy(void *dst, const void *src, int c, size_t n)
{
  unsigned char *d = dst;
  const unsigned char *s = src;
  size_t i;

  for (i = 0; i < n; i++) {
    unsigned char v = s[i];
    d[i] = v;
    if (v == (unsigned char)c) {
      return &d[i+1];
    }
  }
  return NULL;
}

char *__strstr(char *target, char *key) {
    for (; *target; target++) {
	char *p = target;
	for (;;) {
	    char k = *key++, t;
	    if (!k) return p;
	    t = *target++;
	    if (!t) return 0;
	    if (k != t) break;
	}
    }
    return 0;
}

/**
 * @fn size_t strspn(const char *s1, const char *s2)
 * @brief count specified characters in string.
 * @param s1 string.
 * @param s2 charcter set.
 * @return count.
 *
 * @crashcase illegal pointer, non-terminated string
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
size_t strspn(const char *s1, const char *s2)
{
  int char_set[256] = {0};
  size_t len = 0;
  
  while (*s2) {
    char_set[(unsigned char)*s2] = 1;
    s2++;
  }
  while (char_set[(unsigned char)*s1]) {
    len++;
    s1++;
  }
  return len;
}

/**
 * @fn size_t strcspn(const char *s1, const char *s2)
 * @brief count specified characters in string.
 * @param s1 string.
 * @param s2 charcter set.(excluded)
 * @return count.
 *
 * @crashcase illegal pointer, non-terminated string
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
size_t strcspn(const char *s1, const char *s2)
{
  int char_set[256] = {1};
  size_t len = 0;

  while (*s2) {
    char_set[(unsigned char)*s2] = 1;
    s2++;
  }
  while ( ! char_set[(unsigned char)*s1]) {
    len++;
    s1++;
  }
  return len;
}

/**
 * @fn char *strpbrk(const char *s1, const char *s2)
 * @brief find specified characters in string.
 * @param s1 string.
 * @param s2 charcter set.
 * @return count.
 *
 * @crashcase illegal pointer, non-terminated string
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
char *strpbrk(const char *s1, const char *s2)
{
  int char_set[256] = {1};

  while (*s2) {
    char_set[(unsigned char)*s2] = 1;
    s2++;
  }
  while ( ! char_set[(unsigned char)*s1]) {
    s1++;
  }

  if(*s1 == '\0'){
    return NULL;
  }else{
    return (char*)s1;
  }
}

/**
 * @fn char *strdup(const char *s)
 * @brief duplicate string
 * @param s original string
 * @retval !=NULL duplicated string
 * @retval NULL cannot allocate memory
 *
 * @crashcase illegal pointer, non-terminated string
 * @fsctype string(rw)
 *
 * @author Lepidum Co., Ltd.
 */
char *strdup(const char *s)
{
  int len;
  char *r;

  len = strlen(s);
  if(len >= INT_MAX) { /* not happen? */
    return NULL;
  }

  r = (char*)malloc(len + 1);
  if(r == NULL){
    return NULL;
  }else{
    strcpy(r, s);
    return r;
  }
}

/**
 * @fn char *strtok(char *s, const char *sep)
 * @brief tokenize string
 *
 * @author Lepidum Co., Ltd.
 */
char *strtok(char *s, const char *sep)
{
  static char *last_s = NULL;
  char *p;
  int char_set[256] = {0};

  if (s == NULL) {
    s = last_s;
    if (s == NULL) {
      return NULL;
    }
  }

  while (*sep) {
    char_set[(unsigned char)*sep] = 1;
    sep++;
  }

  /* search not contained in sep */
  for (s;; s++) {
    if (*s == 0) {
      last_s = NULL;
      return NULL;
    }
    if (!char_set[(unsigned char)*s]) {
      break;
    }
  }
  /* search contained */
  for (p = s;; p++) {
    if (*p == 0) {
      last_s = NULL;
      return s;
    }
    if (char_set[(unsigned char)*p]) {
      last_s = p + 1;
      *p = 0;
      return s;
    }
  }
}

/**
 * @fn int bcmp(const void *s1, const void *s2, size_t n)
 * @brief compare two memory blocks
 * @param s1 memory block.
 * @param s2 memory block.
 * @retval 0 two blocks are identical.
 * @retval != 0 two blocks are different.
 *
 * @author Lepidum Co., Ltd.
 */
int bcmp(const void *s1, const void *s2, size_t n)
{
  return memcmp(s1, s2, n);
}

/**
 * @fn int ffs(int i)
 * @brief find first set bit
 *
 * @author Lepidum Co., Ltd.
 */
int ffs(int i)
{
  int bit = 0;

  if (i == 0)
    return 0;

  while (1) {
    if ((unsigned int)i >> bit & 1)
      return bit + 1;
    ++bit;
  }
}
