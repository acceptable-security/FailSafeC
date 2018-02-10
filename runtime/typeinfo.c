/* 
   Part of Fail-Safe C Runtime. Produced by Yutaka Oiwa.
   (c) 2001-2005 Yutaka Oiwa.
   (c) 2005-2006 AIST.

   This file is written by Yutaka Oiwa in 2002-2006.

   This file is distributed under Apache License 2.0 with a special exception.
   See the file LICENSE contained in the distribution.

 */

#include <stdlib.h>
#include <typeinfo.h>
#include <primitive_ops.h>

struct typeinfo_init fsc_typeinfo_Xti_ = {
  EMIT_HEADER_FOR_TYPEINFO,
  {
    "__typeinfo",
    TI_SPECIAL | TI_NO_USERALLOC,
    NULL,
    4, 8,
    EMIT_TYPEINFO_ACCESS_METHODS_TABLE_NOACCESS
  }
};

struct typeinfo_init fsc_typeinfo_v = {
  EMIT_HEADER_FOR_TYPEINFO,
  {
    "void",
    TI_SPECIAL | TI_NO_USERALLOC,
    NULL,
    0, 0,
    EMIT_TYPEINFO_ACCESS_METHODS_TABLE_NOACCESS
  }
};

struct typeinfo_init fsc_typeinfo_i = {
  EMIT_HEADER_FOR_TYPEINFO,
  {
    "int",
    TI_PRIMITIVE,
    NULL,
    4, 8,
    read_dword_by_word,
    read_word_fat_int,
    read_hword_by_word,
    read_byte_fat_int,
    write_dword_to_word,
    write_word_fat_int,
    write_hword_to_word,
    write_byte_to_word,
    FSC_ADDITIONAL_HELPERS_DEFAULT
  }
};

struct typeinfo_init fsc_typeinfo_q = {
  EMIT_HEADER_FOR_TYPEINFO,
  {
    "long long", TI_PRIMITIVE,
    NULL,
    8, 16,
    read_dword_fat_longlong,
    read_word_fat_longlong,
    read_hword_by_word,
    read_byte_by_word,
    write_dword_fat_longlong,
    write_word_fat_longlong,
    write_hword_to_word,
    write_byte_to_word,
    FSC_ADDITIONAL_HELPERS_DEFAULT
  }
};

struct typeinfo_init fsc_typeinfo_d = {
  EMIT_HEADER_FOR_TYPEINFO,
  {
    "double", 
    TI_PRIMITIVE | TI_CONTINUOUS,
    NULL,
    8, 8,
    read_dword_continuous,
    read_word_continuous,
    read_hword_continuous,
    read_byte_continuous,
    write_dword_continuous,
    write_word_continuous,
    write_hword_continuous,
    write_byte_continuous,
    FSC_ADDITIONAL_HELPERS_CONTINUOUS
  }
};

struct typeinfo_init fsc_typeinfo_f = {
  EMIT_HEADER_FOR_TYPEINFO,
  {
    "__simple float", TI_PRIMITIVE | TI_CONTINUOUS, NULL,
    4, 4,
    read_dword_continuous,
    read_word_continuous,
    read_hword_continuous,
    read_byte_continuous,
    write_dword_continuous,
    write_word_continuous,
    write_hword_continuous,
    write_byte_continuous,
    FSC_ADDITIONAL_HELPERS_CONTINUOUS
  }
};

/*
struct typeinfo_s typeinfo_simple_longlong = {
  EMIT_HEADER_FOR_TYPEINFO,
  "__simple long long", TI_PRIMITIVE, NULL,
  8, 8,
  read_dword_continuous,
  read_word_continuous,
  read_hword_continuous,
  read_byte_continuous,
  write_dword_continuous,
  write_word_continuous,
  write_hword_continuous,
  write_byte_continuous,
};

struct typeinfo_s typeinfo_simple_int = {
  EMIT_HEADER_FOR_TYPEINFO,
  "__simple int", TI_PRIMITIVE, NULL,
  4, 4,
  read_dword_continuous,
  read_word_continuous,
  read_hword_continuous,
  read_byte_continuous,
  write_dword_continuous,
  write_word_continuous,
  write_hword_continuous,
  write_byte_continuous,
};
*/

struct typeinfo_init fsc_typeinfo_s = {
  EMIT_HEADER_FOR_TYPEINFO,
  {
    "short", TI_PRIMITIVE | TI_CONTINUOUS, NULL,
    2, 2,
    read_dword_continuous,
    read_word_continuous,
    read_hword_continuous,
    read_byte_continuous,
    write_dword_continuous,
    write_word_continuous,
    write_hword_continuous,
    write_byte_continuous,
    FSC_ADDITIONAL_HELPERS_CONTINUOUS
  }
};

struct typeinfo_init fsc_typeinfo_c = {
  EMIT_HEADER_FOR_TYPEINFO,
  {
    "char", TI_PRIMITIVE | TI_CONTINUOUS, NULL,
    1, 1,
    read_dword_continuous,
    read_word_continuous,
    read_hword_continuous,
    read_byte_continuous,
    write_dword_continuous,
    write_word_continuous,
    write_hword_continuous,
    write_byte_continuous,
    FSC_ADDITIONAL_HELPERS_CONTINUOUS
  }
};

