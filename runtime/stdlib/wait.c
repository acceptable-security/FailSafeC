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
 * @file stdlib/wait.c
 */
#define FSC_RUNTIME_LIBRARY
#include <fsc_runtime.h>
#include <wrapper_helper.h>

#include <sys/wait.h>

/**
 * @fn pid_t wait(int *status)
 * @brief wait for child process termination.
 * @param status exit status of the child process is stored, may be a null pointer.
 * @retval >=0 success, pid of the child process is returned.
 * @retval (pid_t)-1 fail.
 *
 * @crashcase invalid pointer value (status), except null.
 * @fsctype pointer(atomic, wo)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FPi_i_wait(base_t status_base0, ofs_t status_ofs)
{
  base_t status_base = base_remove_castflag(status_base0);
  int status;
  pid_t pid;

  pid = wait(&status);

  if(status_base != 0){
    write_word(status_base, status_ofs, value_of_int(status), NULL);
  }

  return value_of_int(pid);
}

/**
 * @fn pid_t waitpid(pid_t pid, int *status, int options)
 * @brief wait for child process termination
 * @param pid child process ID, (pid_t)-1, 0 or minus of process group ID
 * @param status exit status of the child process is stored, may be a null pointer.
 * @param options 0 or any combination of WNOHANG, WUNTRACED and WCONTINUED.
 * @retval >=0 success, pid of the child process is returned.
 * @retval (pid_t)-1 fail.
 *
 * @crashcase invalid pointer value (status), except null.
 * @fsctype pointer(atomic, wo)
 *
 * @author Lepidum Co., Ltd.
 */
value FS_FiPii_i_waitpid(base_t pid_base0, unsigned int pid_value,
                         base_t status_base0, ofs_t status_ofs,
                         base_t options_base0, unsigned int options_value)
{
  base_t status_base = base_remove_castflag(status_base0);
  int status;
  pid_t pid;

  pid = waitpid(pid_value, &status, options_value);

  if(status_base != 0){
    write_word(status_base, status_ofs, value_of_int(status), NULL);
  }

  return value_of_int(pid);
}
