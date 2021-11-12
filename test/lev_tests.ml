open Printf
open Lev

let%expect_test "version" =
  let major, minor = ev_version () in
  printf "version (%d, %d)\n" major minor;
  [%expect {| version (4, 33) |}]

let%expect_test "default" =
  ignore (Loop.default ());
  [%expect {||}]

let%expect_test "create and run" =
  let ev = Loop.create () in
  (match Loop.run ev `No_wait with
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
      (fun _ _ ->
        let b = Bytes.make 1 '0' in
        match Unix.read r b 0 1 with
        | exception Unix.Unix_error (EAGAIN, _, _) -> ()
        | s ->
            assert (s = 1);
            printf "Read char %s\n" (Bytes.to_string b);
            Unix.close r)
      r
      (Io.Event.Set.create ~read:true ())
  in
  let io_w =
    Io.create
      (fun _ _ ->
        ignore (Unix.write w (Bytes.make 1 'c') 0 1);
        print_endline "written to pipe";
        Unix.close w)
      w
      (Io.Event.Set.create ~write:true ())
  in
  Io.start io_r loop;
  Io.start io_w loop;
  ignore (Lev.Loop.run loop `No_wait);
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Unix.Unix_error(Unix.EBADF, \"write\", \"\")")
  Raised by primitive operation at Unix.write in file "unix.ml", line 262, characters 7-34
  Called from Lev_tests.(fun) in file "test/lev_tests.ml", line 41, characters 15-52
  Called from Lev.Loop.run in file "src/lev.ml", line 16, characters 19-24
  Called from Lev.Loop.run in file "src/lev.ml", line 16, characters 19-24
  Called from Lev_tests.(fun) in file "test/lev_tests.ml", line 49, characters 9-37
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  written to pipe |}]
