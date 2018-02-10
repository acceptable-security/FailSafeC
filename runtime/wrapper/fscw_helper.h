#ifndef __FSCW_HELPER_H
#define __FSCW_HELPER_H

#ifndef FSCW_INLINE
#define FSCW_INLINE static inline
#endif

#define fscw_base_ofs(p) p##_base, p##_ofs

#define fscw_is_null(p) (fsc_is_nullpointer((p##_base), (p##_ofs)))
#define fscw_is_not_null(p) (!fsc_is_nullpointer((p##_base), (p##_ofs)))
#define fscw_ensure_nonnull(p, libname) (fsc_ensure_nonnull((p##_base), (p##_ofs), (libname)))

typedef struct {
  void *v;
  void *tmpbuf;
} fscw_buffer;

FSCW_INLINE fscw_buffer fscw_buffer_get_for_read(base_t base, ofs_t ofs, size_t sz, const char *libname)
{
  fscw_buffer b;
  b.v = wrapper_get_read_buffer(base, ofs, &b.tmpbuf, sz, libname);
  return b;
}

FSCW_INLINE fscw_buffer fscw_buffer_get(base_t base, ofs_t ofs, size_t sz, const char *libname)
{
  fscw_buffer b;
  b.v = wrapper_get_rawimage(base, ofs, &b.tmpbuf, sz, libname);
  return b;
}

FSCW_INLINE void fscw_buffer_release(fscw_buffer buffer)
{
  wrapper_release_tmpbuf(buffer.tmpbuf);
}

FSCW_INLINE void fscw_buffer_writeback_release(base_t base, ofs_t ofs, fscw_buffer buffer, size_t sz)
{
  wrapper_writeback_release_tmpbuf(base, ofs, buffer.tmpbuf, sz);
}

extern ptrvalue fscw_string_array_wrap(const char * const p[]);

struct fscw_string_array_buf {
  char **v;
  size_t count;
  int release_v;
  struct fscw_string_buf *buf;
};

extern struct fscw_string_array_buf
fscw_string_array_unwrap_to(char **dst, base_t src_b, ofs_t src_o, size_t count);

#endif
