#include "ev.h"

#include <stdbool.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#define Ev_io_val(v) *(struct ev_io **)Data_custom_val(v)

static int compare_watchers(value a, value b) {
  return (int)((char *)Ev_io_val(a) - (char *)Ev_io_val(b));
}

static long hash_watcher(value watcher) { return (long)Ev_io_val(watcher); }

static struct custom_operations watcher_ops = {
    // TODO free
    "lev.watcher", custom_finalize_default,  compare_watchers,
    hash_watcher,  custom_serialize_default, custom_deserialize_default};

CAMLprim value lev_version(value v_unit) {
  CAMLparam1(v_unit);
  CAMLlocal1(v_version);
  int major = ev_version_major();
  int minor = ev_version_minor();
  v_version = caml_alloc(2, 0);
  Store_field(v_version, 0, Val_int(major));
  Store_field(v_version, 1, Val_int(minor));
  CAMLreturn(v_version);
}

CAMLprim value lev_ev_default(value v_unit) {
  CAMLparam1(v_unit);
  struct ev_loop *loop = ev_default_loop(0);
  CAMLreturn(caml_copy_nativeint((intnat)loop));
}

CAMLprim value lev_ev_create(value v_unit) {
  CAMLparam1(v_unit);
  struct ev_loop *loop = ev_loop_new(ev_recommended_backends());
  if (!loop) {
    caml_failwith("unable to create loop");
  }
  CAMLreturn(caml_copy_nativeint((intnat)loop));
}

CAMLprim value lev_ev_run(value v_ev) {
  CAMLparam1(v_ev);
  struct ev_loop *loop = (struct ev_loop *)Nativeint_val(v_ev);
  caml_release_runtime_system();
  bool ret = ev_run(loop, 0);
  caml_acquire_runtime_system();
  CAMLreturn(Val_bool(ret));
}

static void lev_io_cb(EV_P_ ev_io *w, int revents) {
  caml_acquire_runtime_system();
  caml_callback2((value)w->data, caml_copy_nativeint((intnat)loop),
                 Val_int(revents));
  caml_release_runtime_system();
}

CAMLprim value lev_io_read_code(value v_unit) {
  CAMLparam1(v_unit);
  CAMLreturn(Val_int(EV_READ));
}

CAMLprim value lev_io_write_code(value v_unit) {
  CAMLparam1(v_unit);
  CAMLreturn(Val_int(EV_WRITE));
}

CAMLprim value lev_io_fd(value v_io) {
  CAMLparam1(v_io);
  ev_io *io = Ev_io_val(v_io);
  CAMLreturn(Val_int(io->fd));
}

CAMLprim value lev_io_create(value v_cb, value v_fd, value v_flags) {
  CAMLparam3(v_cb, v_fd, v_flags);
  CAMLlocal2(v_io, v_cb_applied);
  ev_io *io = caml_stat_alloc(sizeof(ev_io));
  ev_io_init(io, lev_io_cb, Int_val(v_fd), Int_val(v_flags));
  v_io = caml_alloc_custom(&watcher_ops, sizeof(struct ev_io *), 0, 1);
  Ev_io_val(v_io) = io;
  v_cb_applied = caml_callback(v_cb, v_io);
  io->data = (void *)v_cb_applied;
  caml_register_generational_global_root((value *)(&(io->data)));
  CAMLreturn(v_io);
}

CAMLprim value lev_io_start(value v_io, value v_ev) {
  CAMLparam2(v_io, v_ev);
  ev_io *io = Ev_io_val(v_io);
  struct ev_loop *ev = (struct ev_loop *)Nativeint_val(v_ev);
  ev_io_start(ev, io);
  CAMLreturn(Val_unit);
}

CAMLprim value lev_io_stop(value v_io, value v_ev) {
  CAMLparam2(v_io, v_ev);
  ev_io *io = Ev_io_val(v_io);
  struct ev_loop *ev = (struct ev_loop *)Nativeint_val(v_ev);
  ev_io_stop(ev, io);
  CAMLreturn(Val_unit);
}
