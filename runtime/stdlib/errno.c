/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa and Lepidum Co. Ltd. in 2004.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.
*/
/**
 * @file stdlib/errno.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>

struct struct_Sn12stdlib_errno_ {base_t base;};

value read_fsc_errno_word(base_t base_c, ofs_t ofs) {
    base_t base = base_remove_castflag(base_c);

    if (ofs != 0)
	fsc_raise_error(base_c, ofs, ERR_OUTOFBOUNDS);
    return value_of_base_vaddr(*(base_t *)base, errno);
}

void write_fsc_errno_word(base_t base_c, ofs_t ofs, value v, typeinfo_t ti) {
    base_t base = base_remove_castflag(base_c);

    if (ofs != 0)
	fsc_raise_error(base_c, ofs, ERR_OUTOFBOUNDS);
    *(base_t *)base = base_of_value(v);
    errno = vaddr_of_value(v);
}

struct typeinfo_init fsc_typeinfo_Sn12stdlib_errno_ = {
    EMIT_HEADER_FOR_TYPEINFO,
    {
	"stdlib_errno",
	TI_SPECIAL,
	NULL,
	4,
	4,
	read_dword_by_word,
	read_fsc_errno_word,
	read_hword_by_word,
	read_byte_by_word,
	write_dword_to_word,
	write_fsc_errno_word,
	write_hword_to_word,
	write_byte_to_word,
	FSC_ADDITIONAL_HELPERS_DEFAULT
    }};

/**
 * @var int errno
 * @author Yutaka Oiwa.
 */
struct  fsc_storage_Sn12stdlib_errno__s 
{
    struct  fsc_header   fsc_header;
    struct  struct_Sn12stdlib_errno_ val;
};

static struct fsc_storage_Sn12stdlib_errno__s GV_Sn12stdlib_errno____errno = {
    EMIT_FSC_HEADER(fsc_typeinfo_Sn12stdlib_errno_.val, 0), {0}
};

/**
 * @fn int *__errno(void)
 */
ptrvalue FS_F_Pi___errno(void)
{
  return ptrvalue_of_base_ofs(
    base_put_castflag((base_t)&GV_Sn12stdlib_errno____errno.val),
    0
    );
}
