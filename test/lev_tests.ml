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
      (fun io loop events ->
        let b = Bytes.make 1 '0' in
        match Unix.read r b 0 1 with
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
      (fun io loop events ->
        assert (Io.Event.Set.mem events Write);
        ignore (Unix.write w (Bytes.make 1 'c') 0 1);
        print_endline "written to pipe";
        Unix.close w;
        Io.stop io loop)
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

  "Assert_failure src/lev.ml:48:20"
  Raised at Lev.Io.Event.Set.mem in file "src/lev.ml", line 48, characters 20-32
  Called from Lev_tests.(fun) in file "test/lev_tests.ml", line 43, characters 15-46
  Called from Lev.Loop.run in file "src/lev.ml", line 16, characters 19-24
  Called from Lev.Loop.run in file "src/lev.ml", line 16, characters 19-24
  Called from Lev_tests.(fun) in file "test/lev_tests.ml", line 53, characters 9-37
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19 |}]
