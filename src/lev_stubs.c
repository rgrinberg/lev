#include "ev.h"

#include <stdbool.h>

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/threads.h>

#define Ev_watcher_val(v) *(struct ev_watcher **)Data_custom_val(v)
#define Ev_io_val(v) *(struct ev_io **)Data_custom_val(v)
#define Ev_timer_val(v) *(struct ev_timer **)Data_custom_val(v)
#define Ev_periodic_val(v) *(struct ev_periodic **)Data_custom_val(v)

#define DEF_BACKEND(__name, __value) CAMLprim value lev_backend_##__name(value v_unit) { CAMLparam1(v_unit); CAMLreturn(Int_val(__value)); }

DEF_BACKEND(poll, EVBACKEND_POLL)
DEF_BACKEND(select, EVBACKEND_SELECT)
DEF_BACKEND(epoll, EVBACKEND_EPOLL)
DEF_BACKEND(kqueue, EVBACKEND_KQUEUE)
DEF_BACKEND(devpoll, EVBACKEND_DEVPOLL)
DEF_BACKEND(port, EVBACKEND_PORT)
DEF_BACKEND(linuxaio, EVBACKEND_LINUXAIO)
DEF_BACKEND(iouring, EVBACKEND_IOURING)

CAMLprim value lev_backend_supported(value v_unit) {
  CAMLparam1(v_unit);
  CAMLreturn(Int_val(ev_supported_backends()));
}

CAMLprim value lev_backend_recommended(value v_unit) {
  CAMLparam1(v_unit);
  CAMLreturn(Int_val(ev_recommended_backends()));
}

CAMLprim value lev_backend_embeddable(value v_unit) {
  CAMLparam1(v_unit);
  CAMLreturn(Int_val(ev_embeddable_backends()));
}

static int compare_watchers(value a, value b) {
  return (int)((char *)Ev_watcher_val(a) - (char *)Ev_watcher_val(b));
}

static long hash_watcher(value watcher) {
  return (long)Ev_watcher_val(watcher);
}

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

CAMLprim value lev_ev_now(value v_ev) {
  CAMLparam1(v_ev);
  struct ev_loop *loop = (struct ev_loop *)Nativeint_val(v_ev);
  ev_tstamp now = ev_now(loop);
  CAMLreturn(caml_copy_double(now));
}

CAMLprim value lev_sleep(value v_ts) {
  CAMLparam1(v_ts);
  ev_sleep(Double_val(v_ts));
  CAMLreturn(Val_unit);
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

static void lev_timer_cb(EV_P_ ev_timer *w, int revents) {
  caml_acquire_runtime_system();
  caml_callback((value)w->data, caml_copy_nativeint((intnat)loop));
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
  caml_remove_generational_global_root((value *)(&(io->data)));
  ev_io_stop(ev, io);
  CAMLreturn(Val_unit);
}

CAMLprim value lev_timer_create(value v_cb, value v_after, value v_repeat) {
  CAMLparam3(v_cb, v_after, v_repeat);
  CAMLlocal2(v_timer, v_cb_applied);
  ev_timer *timer = caml_stat_alloc(sizeof(ev_timer));
  ev_timer_init(timer, lev_timer_cb, Double_val(v_after), Double_val(v_repeat));
  v_timer = caml_alloc_custom(&watcher_ops, sizeof(struct ev_timer *), 0, 1);
  Ev_timer_val(v_timer) = timer;
  v_cb_applied = caml_callback(v_cb, v_timer);
  timer->data = (void *)v_cb_applied;
  caml_register_generational_global_root((value *)(&(timer->data)));
  CAMLreturn(v_timer);
}

CAMLprim value lev_timer_start(value v_timer, value v_ev) {
  CAMLparam2(v_timer, v_ev);
  ev_timer *timer = Ev_timer_val(v_timer);
  struct ev_loop *ev = (struct ev_loop *)Nativeint_val(v_ev);
  ev_timer_start(ev, timer);
  CAMLreturn(Val_unit);
}

CAMLprim value lev_timer_stop(value v_timer, value v_ev) {
  CAMLparam2(v_timer, v_ev);
  ev_timer *timer = Ev_timer_val(v_timer);
  struct ev_loop *ev = (struct ev_loop *)Nativeint_val(v_ev);
  caml_remove_generational_global_root((value *)(&(timer->data)));
  ev_timer_stop(ev, timer);
  CAMLreturn(Val_unit);
}

CAMLprim value lev_timer_remaining(value v_timer, value v_ev) {
  CAMLparam2(v_timer, v_ev);
  ev_timer *timer = Ev_timer_val(v_timer);
  struct ev_loop *ev = (struct ev_loop *)Nativeint_val(v_ev);
  CAMLreturn(ev_timer_remaining(ev, timer));
}

CAMLprim value lev_timer_again(value v_timer, value v_ev) {
  CAMLparam2(v_timer, v_ev);
  ev_timer *timer = Ev_timer_val(v_timer);
  struct ev_loop *ev = (struct ev_loop *)Nativeint_val(v_ev);
  ev_timer_again(ev, timer);
  CAMLreturn(Val_unit);
}

CAMLprim value lev_periodic_create_regular(value v_cb, value v_offset,
                                           value v_interval) {
  caml_failwith("TODO");
}

CAMLprim value lev_periodic_create_custom(value v_cb, value v_reschedule) {
  caml_failwith("TODO");
}

CAMLprim value lev_periodic_start(value v_periodic, value v_ev) {
  caml_failwith("TODO");
}

CAMLprim value lev_periodic_stop(value v_periodic, value v_ev) {
  caml_failwith("TODO");
}
