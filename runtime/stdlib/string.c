/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.
   (c) 2006	 Lepidum Co. Ltd.

   This file is written by Yutaka Oiwa in 2004-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/**
 * @file stdlib/string.c
 */

#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <stdio.h>
#include <stdlib.h>

/**
 * @fn size_t strlen(const char *s)
 * @author Yutaka Oiwa.
 */
value FS_FPc_i_strlen(base_t base0, ofs_t ofs) {
    base_t base;
    fsc_header *header;
    char *p;
    ofs_t ofslimit, i;
    
    fsc_ensure_nonnull(base0, ofs, "strlen");
    base = base_remove_castflag(base0);
    header = get_header_fast(base);
    ofslimit = header->total_ofslimit;
    dealloc_check_fast(base, ofs);

    if (ofslimit <= ofs)
	fsc_raise_error_library(base, ofs, ERR_OUTOFBOUNDS, "strlen");

    if (get_header_fast(base)->tinfo->kind & TI_CONTINUOUS) {
	p = get_realoffset_c(base, ofs);
	for (i = 0; p[i]; i++) {
	    if (ofslimit <= ofs + i)
		fsc_raise_error_library(base0, ofs + i, ERR_OUTOFBOUNDS, "strlen");
	}
	return value_of_int(i);
    } else {
	for(i = 0; read_byte(base, ofs + i) != 0; i++) {
	    if (ofslimit <= ofs + i)
		fsc_raise_error_library(base0, ofs + i, ERR_OUTOFBOUNDS, "strlen");
	}
	return value_of_int(i);
    }
}

extern ptrvalue FS_FPcPc_Pc___strstr(base_t, ofs_t, base_t, ofs_t);

/**
 * @fn char *strstr(const char *s1, const char *s2)
 * @author Yutaka Oiwa.
 */
ptrvalue FS_FPcPc_Pc_strstr(base_t target_b0, ofs_t target_o, base_t key_b0, ofs_t key_o) {
  if ((get_header(target_b0)->tinfo->kind & TI_CONTINUOUS) == 0 ||
      (get_header(key_b0)->tinfo->kind & TI_CONTINUOUS) == 0)
	return FS_FPcPc_Pc___strstr(target_b0, target_o, key_b0, key_o);
    else {
	base_t target_b = base_remove_castflag(target_b0);
	base_t key_b = base_remove_castflag(key_b0);
	int len = 0;
	char *key, *p;

	ofs_t key_ofslimit = get_header(key_b)->total_ofslimit;
	ofs_t target_ofslimit = get_header(target_b)->total_ofslimit;
	if (key_o >= get_header(key_b0)->total_ofslimit)
	    fsc_raise_error_library(key_b0, key_ofslimit, ERR_OUTOFBOUNDS, "strstr");
	
	p = key = get_realoffset_c(key_b, key_o);
	for (len = 0; p[len]; len++) {
	    if (key_ofslimit <= key_o + len)
		fsc_raise_error_library(key_b0, key_o + len, ERR_OUTOFBOUNDS, "strstr");
	}
	if (len == 0)
	    return ptrvalue_of_base_ofs(target_b0, target_o);

	for(;; target_o++) {
	    int i;
	    for (i = 0; i < len; i++) {
		char c;
		if (target_o + i >= target_ofslimit)
		    fsc_raise_error_library(target_b0, target_o + i, ERR_OUTOFBOUNDS, "strstr");
		c = *get_realoffset_c(target_b, target_o + i);
		if (!c)
		    return ptrvalue_of_base_ofs(0, 0);
		if (c != key[i])
		    goto cont;
	    }
	    return ptrvalue_of_base_ofs(target_b0, target_o);
	cont:
	    ;
	}
    }
}
