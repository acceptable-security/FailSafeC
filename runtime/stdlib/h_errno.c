/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa and Lepidum Co. Ltd. in 2004.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/**
 * @file stdlib/h_errno.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <stdio.h>
#include <stdlib.h>
#include <netdb.h>

struct struct_Sn14stdlib_h_errno_ {base_t base;};

value read_fsc_h_errno_word(base_t base_c, ofs_t ofs) {
    base_t base = base_remove_castflag(base_c);

    if (ofs != 0)
	fsc_raise_error(base_c, ofs, ERR_OUTOFBOUNDS);
    return value_of_base_vaddr(*(base_t *)base, h_errno);
}

void write_fsc_h_errno_word(base_t base_c, ofs_t ofs, value v, typeinfo_t ti) {
    base_t base = base_remove_castflag(base_c);

    if (ofs != 0)
	fsc_raise_error(base_c, ofs, ERR_OUTOFBOUNDS);
    *(base_t *)base = base_of_value(v);
    h_errno = vaddr_of_value(v);
}

struct typeinfo_init fsc_typeinfo_Sn14stdlib_h_errno_ = {
    EMIT_HEADER_FOR_TYPEINFO,
    {
	"stdlib_h_errno",
	TI_SPECIAL,
	NULL,
	4,
	4,
	read_dword_by_word,
	read_fsc_h_errno_word,
	read_hword_by_word,
	read_byte_by_word,
	write_dword_to_word,
	write_fsc_h_errno_word,
	write_hword_to_word,
	write_byte_to_word,
	FSC_ADDITIONAL_HELPERS_DEFAULT
    }};

/**
 * @var int h_errno
 * @author Yutaka Oiwa.
 */
struct  fsc_storage_Sn14stdlib_h_errno__s 
{
    struct  fsc_header   fsc_header;
    struct  struct_Sn14stdlib_h_errno_ val;
};

static struct fsc_storage_Sn14stdlib_h_errno__s GV_Sn14stdlib_h_errno____h_errno = {
    EMIT_FSC_HEADER(fsc_typeinfo_Sn14stdlib_h_errno_.val, 0), {0}
};

/**
 * @fn int *__h_errno(void)
 */
ptrvalue FS_F_Pi___h_errno(void)
{
  return ptrvalue_of_base_ofs(
    base_put_castflag((base_t)&GV_Sn14stdlib_h_errno____h_errno.val),
    0
    );
}
