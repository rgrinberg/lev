open! Stdune
open Fiber.O
open Lev_fiber

let%expect_test "pipe" =
  let run () =
    let* input, output = Io.pipe () in
    let write () =
      let f = Faraday.create 100 in
      Faraday.write_string f "foobar";
      let* res = Io.write output f in
      match res with `Yield -> assert false | `Close -> Fiber.return ()
    in
    let read () =
      let+ () =
        Io.with_read input ~f:(fun reader ->
            let buf = Stdlib.Buffer.create 12 in
            let rec loop () =
              match Io.Reader.available reader with
              | `Eof ->
                  let contents = Stdlib.Buffer.contents buf in
                  printfn "reader: %s" contents;
                  Fiber.return ()
              | `Ok 0 ->
                  let* () = Io.Reader.refill reader in
                  loop ()
              | `Ok _ ->
                  let b = Io.Reader.buffer reader in
                  Buffer.add_string buf (Bigstringaf.to_string b);
                  Io.Reader.consume reader ~len:(Bigstringaf.length b);
                  loop ()
            in
            loop ())
      in
      Io.close input
    in
    Fiber.fork_and_join_unit read write
  in
  Lev_fiber.run (Lev.Loop.create ()) ~f:run;
  [%expect.unreachable]
  [@@expect.uncaught_exn
    {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  "Assert_failure lev-fiber/src/lev_fiber.ml:426:4"
  Raised at Lev_fiber.Io.read in file "lev-fiber/src/lev_fiber.ml", line 426, characters 4-16
  Called from Lev_fiber_tests__Reader_writer.(fun).run.read.(fun).loop in file "lev-fiber/test/reader_writer.ml", line 18, characters 26-40
  Called from Lev_fiber_tests__Reader_writer.(fun).run.read in file "lev-fiber/test/reader_writer.ml", line 16, characters 8-611
  Called from Fiber.Execution_context.apply in file "src/fiber/fiber.ml", line 196, characters 9-14
  Re-raised at Stdune__Exn.raise_with_backtrace in file "otherlibs/stdune/exn.ml" (inlined), line 36, characters 27-56
  Called from Stdune__Exn_with_backtrace.reraise in file "otherlibs/stdune/exn_with_backtrace.ml", line 18, characters 33-71
  Called from Fiber.Execution_context.forward_error in file "src/fiber/fiber.ml" (inlined), line 146, characters 4-43
  Called from Fiber.Execution_context.apply in file "src/fiber/fiber.ml", line 197, characters 13-30
  Called from Fiber.fork_and_join_unit in file "src/fiber/fiber.ml", line 318, characters 2-177
  Called from Fiber.Execution_context.apply in file "src/fiber/fiber.ml", line 196, characters 9-14
  Re-raised at Stdune__Exn.raise_with_backtrace in file "otherlibs/stdune/exn.ml" (inlined), line 36, characters 27-56
  Called from Stdune__Exn_with_backtrace.reraise in file "otherlibs/stdune/exn_with_backtrace.ml", line 18, characters 33-71
  Called from Fiber.Execution_context.forward_error in file "src/fiber/fiber.ml" (inlined), line 146, characters 4-43
  Called from Fiber.Execution_context.apply in file "src/fiber/fiber.ml", line 197, characters 13-30
  Called from Fiber.Execution_context.run.(fun) in file "src/fiber/fiber.ml", line 269, characters 8-62
  Called from Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 12, characters 8-11
  Re-raised at Stdune__Exn.protectx in file "otherlibs/stdune/exn.ml", line 18, characters 4-11
  Called from Lev_fiber_tests__Reader_writer.(fun) in file "lev-fiber/test/reader_writer.ml", line 36, characters 2-43
  Called from Expect_test_collector.Make.Instance.exec in file "collector/expect_test_collector.ml", line 244, characters 12-19 |}]
