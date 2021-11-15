open Printf
open Lev

let%expect_test "version" =
  let major, minor = ev_version () in
  printf "version (%d, %d)\n" major minor;
  [%expect {| version (4, 33) |}]

let%expect_test "default" =
  ignore (Loop.default ());
  [%expect {||}]

let%expect_test "now" =
  let loop = Loop.default () in
  let (_ : Timestamp.t) = Loop.now loop in
  [%expect {||}]

let%expect_test "sleep" =
  Timestamp.sleep (Timestamp.of_float 0.1);
  [%expect {||}]

let%expect_test "suspend/resume" =
  let loop = Loop.create () in
  Loop.suspend loop;
  Loop.resume loop;
  [%expect {||}]

let%expect_test "supported backends" =
  let s = Backend.supported () in
  if Backend.Set.mem s Poll then print_endline "poll";
  if Backend.Set.mem s Select then print_endline "select";
  if Backend.Set.mem s Kqueue then print_endline "kqueue";
  [%expect {|
    select
    kqueue|}]

let%expect_test "create and run" =
  let ev = Loop.create () in
  (match Loop.run ev with
  | `No_more_active_watchers -> ()
  | `Otherwise -> assert false);
  [%expect {||}]

let%expect_test "read from pipe" =
  let r, w = Unix.pipe () in
  Unix.set_nonblock r;
  Unix.set_nonblock w;
  let loop = Loop.create () in
  let io_r =
    Io.create
      (fun io fd events ->
        let b = Bytes.make 1 '0' in
        match Unix.read fd b 0 1 with
        | exception Unix.Unix_error (EAGAIN, _, _) -> ()
        | s ->
            assert (Io.Event.Set.mem events Read);
            assert (s = 1);
            printf "read char %s\n" (Bytes.to_string b);
            Unix.close r;
            Io.stop io loop)
      r
      (Io.Event.Set.create ~read:true ())
  in
  let io_w =
    Io.create
      (fun io fd events ->
        assert (Io.Event.Set.mem events Write);
        ignore (Unix.write fd (Bytes.make 1 'c') 0 1);
        print_endline "written to pipe";
        Unix.close w;
        Io.stop io loop)
      w
      (Io.Event.Set.create ~write:true ())
  in
  Io.start io_r loop;
  Io.start io_w loop;
  ignore (Loop.run_until_done loop);
  [%expect {|
    written to pipe
    read char c |}]

let%expect_test "timer" =
  let loop = Loop.create () in
  let timer =
    Timer.create ~after:0.02 (fun timer loop ->
        print_endline "fired timer";
        Timer.stop timer loop)
  in
  Timer.start timer loop;
  ignore (Lev.Loop.run loop);
  [%expect {|
    fired timer |}]
