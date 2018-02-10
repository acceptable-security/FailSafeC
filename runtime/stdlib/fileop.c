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
 * @file stdlib/fileop.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>
#include <fileptr.h>

#include <stdio.h>
#include <stdlib.h>

#if 0
#include <wrapper/fscw.h>
#define FSCW_NEED_fscw_size_t
#define FSCW_NEED_fseek
#define FSCW_NEED_fread
#define FSCW_NEED_fwrite
#include <wrapper/stdlib/stdio.h>
#define fscw_get_FILE_pointer(p) get_FILE_pointer(p##_base, p##_ofs)
#endif

/**
 * @fn FILE *fopen(const char *n, const char *m)
 * @author Yutaka Oiwa.
 */
ptrvalue FS_FPcPc_PSn10stdio_FILE__fopen(base_t n_base, ofs_t n_ofs, 
					 base_t m_base, ofs_t m_ofs)
{
    void *p1, *p2;

    char *n = wrapper_get_string_z(n_base, n_ofs, &p1, "fopen");
    char *m = wrapper_get_string_z(m_base, m_ofs, &p2, "fopen");

    FILE *fp = fopen(n, m);
    base_t p;

    if (fp) {
	p = fsc_alloc_block_library(&fsc_typeinfo_Sn10stdio_FILE_.val, 1);
	*(FILE **)p = fp;
    } else {
	p = 0;
    }
    wrapper_release_tmpbuf(p1);
    wrapper_release_tmpbuf(p2);
    return ptrvalue_of_base_ofs(p, 0);
}

/**
 * @fn int fclose(FILE *fp)
 * @author Yutaka Oiwa.
 */
value FS_FPSn10stdio_FILE__i_fclose(base_t b, ofs_t o) {
    FILE **p;
    int r;

    p = get_FILE_pointer_addr(b, o);
    if (*p == NULL) {
	fsc_raise_error_library(b, o, ERR_INVALIDARGS, "fclose: already closed");
    }
    r = fclose(*p);
    *p = 0;
    return value_of_int(r);
}

/**
 * @fn long ftell(FILE *fp)
 * @author Yutaka Oiwa.
 */
value FS_FPSn10stdio_FILE__i_ftell(base_t b, ofs_t o) {
    FILE *p;
    int r;

    p = get_FILE_pointer(b, o);
    return value_of_int (ftell(p));
}

/**
 * @fn int fseek(FILE *fp, long l, int w)
 * @author Yutaka Oiwa.
 */
#if 1
value FS_FPSn10stdio_FILE_ii_i_fseek(base_t b, ofs_t o,
				     base_t lb, int lo,
				     base_t wb, int wo) {
    FILE *p;
    int r;

    p = get_FILE_pointer(b, o);
    return value_of_int (fseek(p, lo, wo));
}
#else
fscw_int fscw_fseek(fscw_pointer_param(fp_p),
                    fscw_long_param(l),
                    fscw_int_param(w)){
  FILE *fp;

  fp = fscw_get_FILE_pointer(fp_p);
  return fscw_int_wrap(fseek(fp, l, w));
}
#endif

/**
 * @fn size_t fread(void *ptr, size_t size, size_t nmemb, FILE *fp)
 * @author Yutaka Oiwa.
 */
#if 1
value FS_FPviiPSn10stdio_FILE__i_fread(base_t ptr_b, ofs_t ptr_o,
				       base_t size_b, unsigned int size_o,
				       base_t nmemb_b, unsigned int nmemb_o,
				       base_t fp_b, ofs_t fp_o) {
    void *ptr;
    void *p0;
    FILE *fp;
    unsigned int s;
    unsigned int r;

    fp = get_FILE_pointer(fp_b, fp_o);
    if (size_o == 0 || nmemb_o == 0)
	return 0;

    s = size_o * nmemb_o;
    if (s / size_o != nmemb_o) {
	fsc_raise_error_library(0, nmemb_o, ERR_OUTOFBOUNDS, "fread: I/O size exceeds integer");
    }
    ptr = wrapper_get_read_buffer(ptr_b, ptr_o, &p0, s, "fread");
    r = fread(ptr, size_o, nmemb_o, fp);

    assert(r <= nmemb_o);
    wrapper_writeback_release_tmpbuf(ptr_b, ptr_o, p0, r * size_o);
    return value_of_int(r);
}
#else

fscw_int fscw_fread(fscw_pointer_param(ptr_p),
                    fscw_size_t_param(size),
                    fscw_size_t_param(nmemb),
                    fscw_pointer_param(fp_p))
{
  void *ptr;
  FILE *fp;
  size_t s, r;
  fscw_tmpbuf_t *t = NULL;

  fp = fscw_get_FILE_pointer(fp_p);
  if (size == 0 || nmemb == 0) {
    return 0;
  }
  s = size * nmemb;
  if (s / size != nmemb) {
    fsc_raise_error_library(0, nmemb, ERR_OUTOFBOUNDS, "fread: I/O size exceeds integer");
  }
  ptr = fscw_get_buffer_no_copy(ptr_p, &t, s, "fread");
  r = fread(ptr, size, nmemb, fp);

  assert(r <= nmemb);
  fscw_release_buffer_write_back(ptr_p, t, r * size);
  return fscw_size_t_wrap(r);
}
#endif

/**
 * @fn size_t fwrite(const void *ptr, size_t size, size_t nmemb, FILE *fp)
 * @author Yutaka Oiwa.
 */
#if 1
value FS_FPviiPSn10stdio_FILE__i_fwrite(base_t ptr_b, ofs_t ptr_o,
					base_t size_b, unsigned int size_o,
					base_t nmemb_b, unsigned int nmemb_o,
					base_t fp_b, ofs_t fp_o) {
    FILE *fp;
    size_t s;
    void *ptr, *p0;
    int r;
    fp = get_FILE_pointer(fp_b, fp_o);
    if (size_o == 0 || nmemb_o == 0)
	return 0;

    s = size_o * nmemb_o;
    if (s / size_o != nmemb_o) {
	fsc_raise_error_library(0, nmemb_o, ERR_OUTOFBOUNDS, "fwrite: I/O size exceeds integer");
    }
    ptr = wrapper_get_rawimage(ptr_b, ptr_o, &p0, s, "fwrite");
    r = fwrite(ptr, size_o, nmemb_o, fp);
    wrapper_release_tmpbuf(p0);
    return value_of_int(r);
}
#else

fscw_size_t fscw_fwrite(fscw_pointer_param(ptr_p),
                        fscw_size_t_param(size),
                        fscw_size_t_param(nmemb),
                        fscw_pointer_param(fp_p))
{
  void *ptr;
  FILE *fp;
  size_t s, r;
  fscw_tmpbuf_t *t = NULL;

  fp = fscw_get_FILE_pointer(fp_p);
  if (size == 0 || nmemb == 0) {
    return 0;
  }
  s = size * nmemb;
  if (s / size != nmemb) {
    fsc_raise_error_library(0, nmemb, ERR_OUTOFBOUNDS, "fwrite: I/O size exceeds integer");
  }
  ptr = fscw_get_buffer(ptr_p, &t, s, "fwrite");
  r = fwrite(ptr, size, nmemb, fp);

  assert(r <= nmemb);
  fscw_release_buffer(t);
  return fscw_size_t_wrap(r);
}
#endif

/**
 * @fn FILE *popen(const char *n, const char *m)
 * @author Yutaka Oiwa.
 */
ptrvalue FS_FPcPc_PSn10stdio_FILE__popen(base_t n_base, ofs_t n_ofs, 
					 base_t m_base, ofs_t m_ofs)
{
    void *p1, *p2;

    char *n = wrapper_get_string_z(n_base, n_ofs, &p1, "popen");
    char *m = wrapper_get_string_z(m_base, m_ofs, &p2, "popen");

    FILE *fp = popen(n, m);
    base_t p;

    if (fp) {
	p = fsc_alloc_block_library(&fsc_typeinfo_Sn10stdio_FILE_.val, 1);
	*(FILE **)p = fp;
    } else {
	p = 0;
    }
    wrapper_release_tmpbuf(p1);
    wrapper_release_tmpbuf(p2);
    return ptrvalue_of_base_ofs(p, 0);
}

/**
 * @fn int pclose(FILE *fp)
 * @author Yutaka Oiwa.
 */
value FS_FPSn10stdio_FILE__i_pclose(base_t b, ofs_t o) {
    FILE **p;
    int r;

    p = get_FILE_pointer_addr(b, o);
    if (*p == NULL) {
	fsc_raise_error_library(b, o, ERR_INVALIDARGS, "pclose: already closed");
    }
    r = pclose(*p);
    *p = 0;
    return value_of_int(r);
}

/**
 * @fn FILE *fdopen(int fd, const char *mode)
 * @brief get stream associated with specified  file descriptor.
 * @param fd file descrpitor to associate.
 * @param mode mode string as fopen.
 * @retval !=NULL success, new stream returned.
 * @retval NULL error occured.
 * @see fopen(const char *, const char *)
 *
 * @crashcase illegal pointer, non-terminated string
 * @fsctype atomic, string(ro), handle(FILE)
 *
 * @author Lepidum Co., Ltd.
 */
ptrvalue FS_FiPc_PSn10stdio_FILE__fdopen(base_t fd_b, unsigned int fd_o,
                                         base_t mode_b, ofs_t mode_o)
{
  void *tmp;
  char *mode = wrapper_get_string_z(mode_b, mode_o, &tmp, "fdopen");

  FILE *fp = fdopen(fd_o, mode);
  base_t ret;

  if(fp){
    ret = fsc_alloc_block_library(&fsc_typeinfo_Sn10stdio_FILE_.val, 1);
    *(FILE**)ret = fp;
  }else{
    ret = 0;
  }

  wrapper_release_tmpbuf(tmp);
  return ptrvalue_of_base_ofs(ret, 0);
}

/**
 * @fn int feof(FILE *fp)
 * @brief test stream's EOF indicator.
 * @param fp stream to test.
 * @retval !=0 EOF indicator is set.
 * @retval 0 EOF indicator is not set.
 *
 * @crashcase illegal poitner
 * @fsctype atomic, handle(FILE)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPSn10stdio_FILE__i_feof(base_t fp_b, ofs_t fp_o)
{
  FILE *fp = get_FILE_pointer(fp_b, fp_o);
  int ret = feof(fp);
  return value_of_int(ret);
}

/**
 * @fn int ferror(FILE *fp)
 * @brief test error indicator.
 * @param fp stream to test.
 * @retval !=0 error indicator is set.
 * @retval 0 error indicator is not set.
 *
 * @crashcase illegal poitner
 * @fsctype atomic, handle(FILE)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPSn10stdio_FILE__i_ferror(base_t fp_b, ofs_t fp_o)
{
  FILE *fp = get_FILE_pointer(fp_b, fp_o);
  int ret = ferror(fp);
  return value_of_int(ret);
}

/**
 * @fn int fflush(FILE *fp)
 * @brief flush stream.
 * @param fp stream to flush, or NULL to flush all streams.
 * @retval 0 success.
 * @retval EOF error occured.
 *
 * @crashcase illegal poitner
 * @fsctype atomic, handle(FILE)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPSn10stdio_FILE__i_fflush(base_t fp_b0, ofs_t fp_o)
{
  base_t fp_b = base_remove_castflag(fp_b0);
  int ret;
  if(fsc_is_nullpointer(fp_b0, fp_o)) {
    ret = fflush(NULL);
  }else{
    FILE *fp = get_FILE_pointer(fp_b, fp_o);
    ret = fflush(fp);
  }
  return value_of_int(ret);
}

/**
 * @fn int fileno(FILE *fp)
 * @brief get filedescriptor associated with stream.
 * @param fp stream.
 * @retval -1 error occured.
 * @retval other success
 *
 * @crashcase illegal poitner
 * @fsctype atomic, handle(FILE)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPSn10stdio_FILE__i_fileno(base_t fp_b, ofs_t fp_o)
{
  FILE *fp = get_FILE_pointer(fp_b, fp_o);
  int ret = fileno(fp);
  return value_of_int(ret);
}

/**
 * @fn int setvbuf(FILE *fp, char *buf, int mode, size_t size);
 * @brief set buffering mode of stream.
 * @param fp file pointer.
 * @param buf buffer to assign.
 * @param mode buffering mode.
 * @param size size of buf.
 * @retval 0 success.
 * @retval !=0 error.
 * @todo support non-null buffer.
 *
 * @crashcase illegal pointer, non-null buffer.
 * @fsctype special
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPSn10stdio_FILE_Pcii_i_setvbuf(base_t fp_b, ofs_t fp_o,
                                         base_t buf_b, ofs_t buf_o,
                                         base_t mode_b, unsigned int mode_o,
                                         base_t size_b, unsigned int size_o)
{
  FILE *fp = get_FILE_pointer(fp_b, fp_o);
  int ret = setvbuf(fp, NULL, mode_o, size_o);
  return value_of_int(ret);
}

/**
 * @fn int fgetpos(FILE *fp, fpos_t *pos)
 * @brief get current file position
 * @param fp file pointer.
 * @param pos pointer to fpos_t.
 * @retval 0 success.
 * @retval !=0 error.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype handle(FILE), memory(wo)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPSn10stdio_FILE_PSn13stdlib_fpos_t__i_fgetpos(base_t fp_b, ofs_t fp_o,
                                                        base_t pos_b, ofs_t pos_o
                                                        )
{
  FILE *fp;
  fpos_t *pos;
  void *tmp;
  int ret;

  fp = get_FILE_pointer(fp_b, fp_o);

  pos = (fpos_t*)wrapper_get_read_buffer(pos_b, pos_o, &tmp, sizeof(fpos_t), "fgetpos");
  ret = fgetpos(fp, pos);

  if(ret == 0){
    wrapper_writeback_release_tmpbuf(pos_b, pos_o, tmp, sizeof(fpos_t));
  }else{
    wrapper_release_tmpbuf(tmp);
  }
  return value_of_int(ret);
}

/**
 * @fn int fsetpos(FILE *fp, const fpos_t *pos)
 * @brief set current file position
 * @param fp file pointer.
 * @param pos pointer to fpos_t.
 * @retval 0 success.
 * @retval !=0 error.
 *
 * @crashcase illegal pointer, buffer overflow
 * @fsctype handle(FILE), memory(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPSn10stdio_FILE_PSn13stdlib_fpos_t__i_fsetpos(base_t fp_b, ofs_t fp_o,
                                                        base_t pos_b, ofs_t pos_o
                                                        )
{
  FILE *fp;
  fpos_t *pos;
  void *tmp;
  int ret;

  fp = get_FILE_pointer(fp_b, fp_o);

  pos = (fpos_t*)wrapper_get_rawimage(pos_b, pos_o, &tmp, sizeof(fpos_t), "fsetpos");
  ret = fsetpos(fp, pos);

  wrapper_release_tmpbuf(tmp);
  return value_of_int(ret);
}

/**
 * @fn int remove(const char *path)
 * @brief remove specified file
 * @param path filename to remove
 * @retval 0 success.
 * @retval -1 failed.
 *
 * @crashcase illegal pointer, non-terminated string.
 * @fsctype string(ro)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPc_i_remove(base_t path_b, ofs_t path_o)
{
  void *tmp;
  char *path = wrapper_get_string_z(path_b, path_o, &tmp, "remove");

  int ret = remove(path);

  wrapper_release_tmpbuf(tmp);
  return value_of_int(ret);
}

/**
 * @fn void clearerr(FILE *fp)
 * @brief clear error flag
 * @param fp file stream
 *
 * @crashcase illegal poitner
 * @fsctype handle(FILE)
 *
 * @author Lepidum Co., Ltd.
 */
void FS_FPSn10stdio_FILE__v_clearerr(base_t fp_b, ofs_t fp_o)
{
  FILE *fp = get_FILE_pointer(fp_b, fp_o);
  clearerr(fp);
}
