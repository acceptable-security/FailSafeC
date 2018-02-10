#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <private_storage.h>

struct typeinfo_init fsc_typeinfo_Sn22stdlib_private_storage_ = {
  EMIT_HEADER_FOR_TYPEINFO,
  {
    "stdlib_private_storage",
    TI_SPECIAL,
    NULL,
    8,
    sizeof (void *),
    EMIT_TYPEINFO_ACCESS_METHODS_TABLE_NOACCESS
  }
};

static void **get_pointer(base_t b, ofs_t o, const void *tag)
{
  base_t br;
  fsc_header *h;

  br = base_remove_castflag(b);
  if (b == 0)
    fsc_raise_error_library(b, o, ERR_NULLPTR, "private_storage");
  h = get_header_fast(br);
  if (h->tinfo != &fsc_typeinfo_Sn22stdlib_private_storage_.val)
    fsc_raise_error_library(b, o, ERR_TYPEMISMATCH, "private_storage");
  if (o != 0)
    fsc_raise_error_library(b, o, ERR_OUTOFBOUNDS, "private_storage");
  if (*((const void **)b+1) != tag)
    fsc_raise_error_library(b, o, ERR_TYPEMISMATCH, "private_storage");
  return (void **)b;
}

void set_private_storage(base_t b, ofs_t o, void *v, const void *tag)
{
  void **p = get_pointer(b, o, tag);
  *p = v;
}

void *get_private_storage(base_t b, ofs_t o, const void *tag)
{
  void **p = get_pointer(b, o, tag);
  return *p;
}

base_t alloc_private_storage(void *init_v, const void *tag)
{
  base_t p = fsc_alloc_block_library(&fsc_typeinfo_Sn22stdlib_private_storage_.val, 1);
  *(void **)p = init_v;
  *((const void **)p+1) = tag;
  return p;
}
