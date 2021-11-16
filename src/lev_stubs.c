#include "ev.h"

#include <stdbool.h>

#include <sys/types.h>
#include <sys/wait.h>

#define TAG_WEXITED 0
#define TAG_WSIGNALED 1
#define TAG_WSTOPPED 2

#define CAML_INTERNALS

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/threads.h>

#define Ev_val(__typ, __v) *(struct __typ **)Data_custom_val(__v)
#define Ev_watcher_val(v) *(struct ev_watcher **)Data_custom_val(v)
#define Ev_io_val(v) *(struct ev_io **)Data_custom_val(v)
#define Ev_child_val(v) *(struct ev_child **)Data_custom_val(v)
#define Ev_timer_val(v) *(struct ev_timer **)Data_custom_val(v)
#define Ev_periodic_val(v) *(struct ev_periodic **)Data_custom_val(v)
#define Ev_cleanup_val(v) *(struct ev_cleanup **)Data_custom_val(v)

#define Cb_for(__t)                                                            \
  (void (*)(struct ev_loop *, struct __t *, int)) lev_watcher_cb

#define DEF_CONST(__name, __value)                                             \
  CAMLprim value __name(value v_unit) {                                        \
    CAMLparam1(v_unit);                                                        \
    CAMLreturn(Int_val(__value));                                              \
  }

#define DEF_BACKEND(__name, __value)                                           \
  CAMLprim value lev_backend_##__name(value v_unit) {                          \
    CAMLparam1(v_unit);                                                        \
    CAMLreturn(Int_val(__value));                                              \
  }

DEF_CONST(lev_loop_break_cancel_code, EVBREAK_CANCEL)
DEF_CONST(lev_loop_break_one_code, EVBREAK_ONE)
DEF_CONST(lev_loop_break_all_code, EVBREAK_ALL)

DEF_BACKEND(poll, EVBACKEND_POLL)
DEF_BACKEND(select, EVBACKEND_SELECT)
DEF_BACKEND(epoll, EVBACKEND_EPOLL)
DEF_BACKEND(kqueue, EVBACKEND_KQUEUE)
DEF_BACKEND(devpoll, EVBACKEND_DEVPOLL)
DEF_BACKEND(port, EVBACKEND_PORT)
DEF_BACKEND(linuxaio, EVBACKEND_LINUXAIO)
DEF_BACKEND(iouring, EVBACKEND_IOURING)

#define DEF_BACKEND_SET(__name, __value)                                       \
  CAMLprim value lev_backend_##__name(value v_unit) {                          \
    CAMLparam1(v_unit);                                                        \
    CAMLreturn(Int_val(__value()));                                            \
  }

DEF_BACKEND_SET(supported, ev_supported_backends)
DEF_BACKEND_SET(recommended, ev_recommended_backends)
DEF_BACKEND_SET(embeddable, ev_embeddable_backends)

#define DEF_STOP(__name)                                                       \
  CAMLprim value lev_##__name##_stop(value v_w, value v_ev) {                  \
    CAMLparam2(v_w, v_ev);                                                     \
    ev_##__name *w = Ev_val(ev_##__name, v_w);                                 \
    struct ev_loop *ev = (struct ev_loop *)Nativeint_val(v_ev);                \
    ev_##__name##_stop(ev, w);                                                 \
    CAMLreturn(Val_unit);                                                      \
  }

DEF_STOP(cleanup)
DEF_STOP(io)
DEF_STOP(timer)
DEF_STOP(periodic)
DEF_STOP(child)
DEF_STOP(signal)
DEF_STOP(stat)
DEF_STOP(embed)
DEF_STOP(idle)
DEF_STOP(check)
DEF_STOP(async)
DEF_STOP(prepare)

#define DEF_START(__name)                                                      \
  CAMLprim value lev_##__name##_start(value v_w, value v_ev) {                 \
    CAMLparam2(v_w, v_ev);                                                     \
    ev_##__name *w = Ev_val(ev_##__name, v_w);                                 \
    struct ev_loop *ev = (struct ev_loop *)Nativeint_val(v_ev);                \
    ev_##__name##_start(ev, w);                                                \
    CAMLreturn(Val_unit);                                                      \
  }

DEF_START(cleanup)
DEF_START(io)
DEF_START(timer)
DEF_START(periodic)
DEF_START(child)
DEF_START(signal)
DEF_START(stat)
DEF_START(embed)
DEF_START(idle)
DEF_START(check)
DEF_START(async)
DEF_START(prepare)

static int compare_watchers(value a, value b) {
  return (int)((char *)Ev_watcher_val(a) - (char *)Ev_watcher_val(b));
}

static long hash_watcher(value watcher) {
  return (long)Ev_watcher_val(watcher);
}

static void finalize_watcher(value v_watcher) {
  ev_watcher *w = Ev_watcher_val(v_watcher);
  caml_remove_generational_global_root((value *)(&(w->data)));
  caml_stat_free(w);
}

static struct custom_operations watcher_ops = {
    "lev.watcher", finalize_watcher,         compare_watchers,
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

CAMLprim value lev_feed_signal(value v_sig) {
  CAMLparam1(v_sig);
  ev_feed_signal(Int_val(v_sig));
  CAMLreturn(Val_unit);
}

CAMLprim value lev_loop_backend(value v_loop) {
  CAMLparam1(v_loop);
  struct ev_loop *loop = (struct ev_loop *)Nativeint_val(v_loop);
  CAMLreturn(Val_int(ev_backend(loop)));
}

CAMLprim value lev_loop_depth(value v_loop) {
  CAMLparam1(v_loop);
  struct ev_loop *loop = (struct ev_loop *)Nativeint_val(v_loop);
  CAMLreturn(Val_int(ev_depth(loop)));
}

CAMLprim value lev_loop_now_update(value v_loop) {
  CAMLparam1(v_loop);
  struct ev_loop *loop = (struct ev_loop *)Nativeint_val(v_loop);
  ev_now_update(loop);
  CAMLreturn(Val_unit);
}

CAMLprim value lev_loop_suspend(value v_loop) {
  CAMLparam1(v_loop);
  struct ev_loop *loop = (struct ev_loop *)Nativeint_val(v_loop);
  ev_suspend(loop);
  CAMLreturn(Val_unit);
}

CAMLprim value lev_loop_resume(value v_loop) {
  CAMLparam1(v_loop);
  struct ev_loop *loop = (struct ev_loop *)Nativeint_val(v_loop);
  ev_resume(loop);
  CAMLreturn(Val_unit);
}

CAMLprim value lev_loop_destroy(value v_loop) {
  CAMLparam1(v_loop);
  struct ev_loop *loop = (struct ev_loop *)Nativeint_val(v_loop);
  ev_loop_destroy(loop);
  CAMLreturn(Val_unit);
}

CAMLprim value lev_loop_break(value v_loop, value v_break) {
  CAMLparam2(v_loop, v_break);
  struct ev_loop *loop = (struct ev_loop *)Nativeint_val(v_loop);
  ev_break(loop, Int_val(v_break));
  CAMLreturn(Val_unit);
}

CAMLprim value lev_ev_default(value v_unit) {
  CAMLparam1(v_unit);
  struct ev_loop *loop = ev_default_loop(0);
  if (!loop) {
    caml_failwith("unable to create loop");
  }
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
  bool ret = ev_run(loop, EVRUN_ONCE);
  caml_acquire_runtime_system();
  ev_invoke_pending(loop);
  CAMLreturn(Val_bool(ret));
}

static void lev_io_cb(EV_P_ ev_io *w, int revents) {
  int fd = w->fd;
  caml_callback2((value)w->data, Val_int(fd), Val_int(revents));
}

static void lev_child_cb(EV_P_ ev_child *w, int revents) {
  CAMLparam0();
  CAMLlocal1(v_status);
  int status = w->rstatus;
  if (WIFEXITED(status)) {
    v_status = caml_alloc_small(1, TAG_WEXITED);
    Field(v_status, 0) = Val_int(WEXITSTATUS(status));
  } else if (WIFSTOPPED(status)) {
    v_status = caml_alloc_small(1, TAG_WSTOPPED);
    Field(v_status, 0) =
        Val_int(caml_rev_convert_signal_number(WSTOPSIG(status)));
  } else {
    v_status = caml_alloc_small(1, TAG_WSIGNALED);
    Field(v_status, 0) =
        Val_int(caml_rev_convert_signal_number(WTERMSIG(status)));
  }
  caml_callback2((value)w->data, Val_int(w->rpid), v_status);
  CAMLdrop;
}

static void lev_watcher_cb(EV_P_ ev_watcher *w, int revents) {
  caml_callback((value)w->data, Val_unit);
}

struct periodic_cbs {
  value watcher;
  value reschedule;
};

static ev_tstamp lev_periodic_reschedule_cb(ev_periodic *w, ev_tstamp now) {
  // TODO do we need this?
  CAMLparam0();
  CAMLlocal1(v_stamp);
  v_stamp = caml_callback((value)w->data, caml_copy_double(now));
  double result = Double_val(v_stamp);
  CAMLdrop;
  return result;
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

CAMLprim value lev_timer_create(value v_cb, value v_after, value v_repeat) {
  CAMLparam3(v_cb, v_after, v_repeat);
  CAMLlocal2(v_timer, v_cb_applied);
  ev_timer *timer = caml_stat_alloc(sizeof(ev_timer));
  ev_timer_init(timer, Cb_for(ev_timer), Double_val(v_after),
                Double_val(v_repeat));
  v_timer = caml_alloc_custom(&watcher_ops, sizeof(struct ev_timer *), 0, 1);
  Ev_timer_val(v_timer) = timer;
  v_cb_applied = caml_callback(v_cb, v_timer);
  timer->data = (void *)v_cb_applied;
  caml_register_generational_global_root((value *)(&(timer->data)));
  CAMLreturn(v_timer);
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
  CAMLparam3(v_cb, v_offset, v_interval);
  CAMLlocal2(v_periodic, v_cb_applied);
  ev_periodic *periodic = caml_stat_alloc(sizeof(ev_periodic));
  ev_periodic_init(periodic, Cb_for(ev_periodic), Int_val(v_offset),
                   Int_val(v_interval), NULL);
  v_periodic =
      caml_alloc_custom(&watcher_ops, sizeof(struct ev_periodic *), 0, 1);
  Ev_periodic_val(v_periodic) = periodic;
  v_cb_applied = caml_callback(v_cb, v_periodic);
  periodic->data = (void *)v_cb_applied;
  caml_register_generational_global_root((value *)(&(periodic->data)));
  CAMLreturn(v_periodic);
}

CAMLprim value lev_periodic_create_custom(value v_cb, value v_reschedule) {
  CAMLparam2(v_cb, v_reschedule);
  CAMLlocal2(v_periodic, v_cb_applied);
  ev_periodic *periodic = caml_stat_alloc(sizeof(ev_periodic));
  ev_periodic_init(periodic, Cb_for(ev_periodic), 0, 0,
                   lev_periodic_reschedule_cb);
  v_periodic =
      caml_alloc_custom(&watcher_ops, sizeof(struct ev_periodic *), 0, 1);
  Ev_periodic_val(v_periodic) = periodic;
  v_cb_applied = caml_callback(v_cb, v_periodic);
  periodic->data = (void *)v_cb_applied;
  caml_register_generational_global_root((value *)(&(periodic->data)));
  CAMLreturn(v_periodic);
}

#define DEF_SIMPLE_CREATE(__name)                                              \
  CAMLprim value lev_##__name##_create(value v_cb) {                           \
    CAMLparam1(v_cb);                                                          \
    CAMLlocal2(v_w, v_cb_applied);                                             \
    ev_##__name *w = caml_stat_alloc(sizeof(ev_##__name));                     \
    ev_##__name##_init(w, Cb_for(ev_##__name));                                \
    v_w = caml_alloc_custom(&watcher_ops, sizeof(struct ev_##__name *), 0, 1); \
    Ev_val(ev_##__name, v_w) = w;                                              \
    v_cb_applied = caml_callback(v_cb, v_w);                                   \
    w->data = (void *)v_cb_applied;                                            \
    caml_register_generational_global_root((value *)(&(w->data)));             \
    CAMLreturn(v_w);                                                           \
  }

DEF_SIMPLE_CREATE(cleanup)
DEF_SIMPLE_CREATE(async)
DEF_SIMPLE_CREATE(check)
DEF_SIMPLE_CREATE(prepare)
DEF_SIMPLE_CREATE(idle)

CAMLprim value lev_child_create(value v_cb, value v_pid, value v_trace) {
  CAMLparam3(v_cb, v_pid, v_trace);
  int pid = Int_val(v_pid);
  int trace = Int_val(v_trace);
  CAMLlocal2(v_child, v_cb_applied);
  ev_child *child = caml_stat_alloc(sizeof(ev_child));
  ev_child_init(child, lev_child_cb, pid, trace);
  v_child = caml_alloc_custom(&watcher_ops, sizeof(struct ev_child *), 0, 1);
  Ev_child_val(v_child) = child;
  v_cb_applied = caml_callback(v_cb, v_child);
  child->data = (void *)v_cb_applied;
  caml_register_generational_global_root((value *)(&(child->data)));
  CAMLreturn(v_child);
}

CAMLprim value lev_embed_sweep(value v_embed, value v_loop) {
  CAMLparam2(v_loop, v_embed);
  struct ev_loop *loop = (struct ev_loop *)Nativeint_val(v_loop);
  ev_embed *embed = Ev_val(ev_embed, v_embed);
  ev_embed_sweep(loop, embed);
  CAMLreturn(Val_unit);
}

CAMLprim value lev_signal_create(value v_cb, value v_signal) {
  CAMLparam2(v_cb, v_signal);
  CAMLlocal2(v_w, v_cb_applied);
  ev_signal *w = caml_stat_alloc(sizeof(ev_signal));
  ev_signal_init(w, Cb_for(ev_signal), Int_val(v_signal));
  v_w = caml_alloc_custom(&watcher_ops, sizeof(struct ev_signal *), 0, 1);
  Ev_val(ev_signal, v_w) = w;
  v_cb_applied = caml_callback(v_cb, v_w);
  w->data = (void *)v_cb_applied;
  caml_register_generational_global_root((value *)(&(w->data)));
  CAMLreturn(v_w);
}

CAMLprim value lev_async_send(value v_async, value v_loop) {
  CAMLparam2(v_loop, v_async);
  struct ev_loop *loop = (struct ev_loop *)Nativeint_val(v_loop);
  ev_async *async = Ev_val(ev_async, v_async);
  ev_async_send(loop, async);
  CAMLreturn(Val_unit);
}

CAMLprim value lev_async_pending(value v_async) {
  CAMLparam1(v_async);
  ev_async *async = Ev_val(ev_async, v_async);
  CAMLreturn(Val_bool(ev_async_pending(async)));
}

CAMLprim value lev_stat_create(value v_cb, value v_path, value v_interval) {
  CAMLparam3(v_cb, v_path, v_interval);
  CAMLlocal2(v_w, v_cb_applied);
  ev_stat *w = caml_stat_alloc(sizeof(ev_stat));
  const char *path = String_val(v_path);
  float interval = Double_val(v_interval);
  ev_stat_init(w, Cb_for(ev_stat), path, interval);
  v_w = caml_alloc_custom(&watcher_ops, sizeof(struct ev_stat *), 0, 1);
  Ev_val(ev_stat, v_w) = w;
  v_cb_applied = caml_callback(v_cb, v_w);
  w->data = (void *)v_cb_applied;
  caml_register_generational_global_root((value *)(&(w->data)));
  CAMLreturn(v_w);
}
