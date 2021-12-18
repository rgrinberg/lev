/* This file is part of Lwt, released under the MIT license. See LICENSE.md for
   details, or visit https://github.com/ocsigen/lwt/blob/master/LICENSE.md. */

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>
#include <caml/unixsupport.h>
#include <unistd.h>

#if _WIN32

#else

CAMLprim value lev_fiber_unix_write(value v_fd, value v_buf, value v_ofs,
                                    value v_len) {
  CAMLparam4(v_fd, v_buf, v_ofs, v_len);
  char *buf = (char *)Caml_ba_array_val(v_buf)->data + Long_val(v_ofs);
  caml_release_runtime_system();
  long ret = write(Int_val(v_fd), buf, Long_val(v_len));
  caml_acquire_runtime_system();
  if (ret == -1)
    uerror("write", Nothing);
  CAMLreturn(Val_long(ret));
}

CAMLprim value lev_fiber_unix_read(value v_fd, value v_buf, value v_ofs,
                                   value v_len) {
  CAMLparam4(v_fd, v_buf, v_ofs, v_len);
  char *buf = (char *)Caml_ba_array_val(v_buf)->data + Long_val(v_ofs);
  caml_release_runtime_system();
  long ret = read(Int_val(v_fd), buf, Long_val(v_len));
  caml_acquire_runtime_system();
  if (ret == -1)
    uerror("read", Nothing);
  CAMLreturn(Val_long(ret));
}

#endif
