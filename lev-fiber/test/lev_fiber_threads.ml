open Stdune
open Fiber.O
open Lev_fiber

let%expect_test "create thread" =
  let f () =
    let* thread = Thread.create () in
    let* task = Thread.task thread ~f:(fun () -> print_endline "in thread") in
    let+ result = Thread.await task in
    match result with Error _ -> assert false | Ok () -> ()
  in
  run (Lev.Loop.create ()) ~f;
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  "Assert_failure src/fiber/fiber.ml:126:6"
  Raised at Fiber.Execution_context.K.enqueue in file "src/fiber/fiber.ml", line 126, characters 6-42
  Called from Stdlib__Queue.iter.iter in file "queue.ml", line 121, characters 6-15
  Called from Stdlib__List.iter in file "list.ml", line 110, characters 12-15
  Called from Fiber.Scheduler.advance in file "src/fiber/fiber.ml" (inlined), line 980, characters 4-23
  Called from Lev_fiber.run.stalled in file "lev-fiber/src/lev_fiber.ml", line 361, characters 25-52
  Called from Lev_fiber_tests__Lev_fiber_threads.(fun) in file "lev-fiber/test/lev_fiber_threads.ml", line 12, characters 2-29
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19

  Trailing output
  ---------------
  in thread |}]
