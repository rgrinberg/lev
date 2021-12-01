open Stdune
open Fiber.O
open Lev_fiber

let%expect_test "create thread" =
  let f () =
    let* thread = Thread.create () in
    let task = Thread.task thread ~f:(fun () -> print_endline "in thread") in
    let+ result = Thread.await task in
    match result with Error _ -> assert false | Ok () -> ()
  in
  run (Lev.Loop.create ()) ~f;
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("(\"deadlock\", {})")
  Raised at Stdune__Code_error.raise in file "otherlibs/stdune/code_error.ml", line 11, characters 30-62
  Called from Lev_fiber_tests__Lev_fiber_threads.(fun) in file "lev-fiber/test/lev_fiber_threads.ml", line 12, characters 2-29
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19 |}]
