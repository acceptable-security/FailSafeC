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
 * @file stdlib/dirent.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>
#include <fileptr.h>

#include <dirent.h>
#include <string.h>

struct struct_Sn14__stdio_dirent_
{
  union  fsc_initU_i   d_ino;
  unsigned char d_name[1025];
  unsigned char __pad1[3];
  int : 32;
};

extern struct typeinfo_init fsc_typeinfo_Sn14__stdio_dirent_;

/**
 * @fn DIR *opendir(const char *path)
 * @brief open directory stream.
 * @param path directory name to open.
 * @retval !=NULL operation successed.
 * @retval NULL operation failed.
 *
 * @crashcase illegal pointer, non-terminated string
 * @fsctype string(ro), handle(DIR)
 *
 * @author Lepidum Co., Ltd.
 */
ptrvalue FS_FPc_PSn10stdio_FILE__opendir(base_t path_b, ofs_t path_o)
{
  void *tmp;
  char *path = wrapper_get_string_z(path_b, path_o, &tmp, "opendir");

  DIR *dir = opendir(path);
  base_t p;

  if(dir){
    p = fsc_alloc_block_library(&fsc_typeinfo_Sn10stdio_FILE_.val, 1);
    *(DIR**)p = dir;
  }else{
    p = 0;
  }

  wrapper_release_tmpbuf(tmp);
  return ptrvalue_of_base_ofs(p, 0);
}

/**
 * @fn int closedir(DIR *dir)
 * @brief close directory stream.
 * @param dir directory stream to close.
 * @retval 0 operation successed.
 * @retval !=0 operation failed.
 *
 * @crashcase illegal pointer
 * @fsctype handle(DIR)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPSn10stdio_FILE__i_closedir(base_t dir_b, ofs_t dir_o)
{
  int r;
  DIR **dir = (DIR**)get_FILE_pointer_addr(dir_b, dir_o);

  if (*dir == NULL) {
    fsc_raise_error_library(dir_b, dir_o, ERR_INVALIDARGS, "closedir: already closed");
  }
  r = closedir(*dir);
  *dir = 0;
  return value_of_int(r);
}

/**
 * @fn struct dirent *readdir(DIR *dir)
 * @brief read directory stream.
 * @param dir directory stream.
 * @retval !=NULL pointer to struct dirent returned.
 * @retval NULL end of directory. (errno not changed)
 * @retval NULL operation failed. (errrno is set)
 *
 * @crashcase illegal pointer
 * @fsctype handle(DIR), struct(atomic)
 *
 * @author Lepidum Co., Ltd.
 */
ptrvalue FS_FPSn10stdio_FILE__PSn14__stdio_dirent__readdir(base_t dir_b, ofs_t dir_o)
{
  DIR *dir = (DIR*)get_FILE_pointer(dir_b, dir_o);
  struct dirent *result;

  result = readdir(dir);

  if(result){
    base_t p_dirent;
    struct struct_Sn14__stdio_dirent_ *dst_dirent;

    p_dirent = fsc_alloc_block_library(&fsc_typeinfo_Sn14__stdio_dirent_.val, 1);
    dst_dirent = (struct struct_Sn14__stdio_dirent_ *)p_dirent;
    dst_dirent->d_ino.cv = result->d_ino;
    /*
     * TODO
     * length of d_name is enough?
     */
    strcpy(dst_dirent->d_name, result->d_name);
    return ptrvalue_of_base_ofs(p_dirent, 0);
  }else{
    return ptrvalue_of_base_ofs(0, 0);
  }
}

/**
 * @fn void rewinddir(DIR *dir)
 * @brief rewind directory stream position
 * @param dir directory stream
 *
 * @crashcase illegal poitner
 * @fsctype handle(DIR)
 *
 * @author Lepidum Co., Ltd.
 */
void FS_FPSn10stdio_FILE__v_rewinddir(base_t dir_b, ofs_t dir_o)
{
  DIR *dir = (DIR*)get_FILE_pointer(dir_b, dir_o);
  rewinddir(dir);
}

/**
 * @fn void seekdir(DIR *dir, long loc)
 * @brief seek directory stream
 * @param dir directory stream
 * @param loc position
 *
 * @crashcase illegal poitner
 * @fsctype handle(DIR)
 *
 * @author Lepidum Co., Ltd.
 */
void FS_FPSn10stdio_FILE_i_v_seekdir(base_t dir_b, ofs_t dir_o, base_t loc_b, long loc)
{
  DIR *dir = (DIR*)get_FILE_pointer(dir_b, dir_o);
  seekdir(dir, loc);
}

/**
 * @fn long telldir(DIR *dir)
 * @brief directory stream position
 * @param dir directory stream
 *
 * @crashcase illegal poitner
 * @fsctype handle(DIR)
 *
 * @author Lepidum Co., Ltd.
 */
long FS_FPSn10stdio_FILE__i_telldir(base_t dir_b, ofs_t dir_o)
{
  DIR *dir = (DIR*)get_FILE_pointer(dir_b, dir_o);
  return value_of_int(telldir(dir));
}
