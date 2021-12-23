open! Stdune
open Fiber.O
open Lev_fiber

let%expect_test "pipe" =
  let run () =
    let* input, output = Io.pipe ~cloexec:true () in
    let write () =
      let+ () =
        Io.with_write output ~f:(fun writer ->
            Io.Writer.add_string writer "foobar";
            Io.Writer.flush writer)
      in
      Io.close output;
      printfn "writer: finished"
    in
    let read () =
      let+ contents = Io.with_read input ~f:Io.Reader.to_string in
      printfn "read: %S" contents;
      Io.close input
    in
    Fiber.fork_and_join_unit read write
  in
  let print_errors f () =
    Fiber.with_error_handler
      ~on_error:(fun exn ->
        Format.eprintf "%a@." Exn_with_backtrace.pp_uncaught exn;
        Exn_with_backtrace.reraise exn)
      f
  in
  Lev_fiber.run (Lev.Loop.create ()) ~f:(print_errors run);
  [%expect {|
    writer: finished
    read: "foobar" |}]

let%expect_test "write with resize" =
  let run () =
    let* input, output = Io.pipe ~cloexec:true () in
    let len = 6120 in
    let write () =
      let+ () =
        Io.with_write output ~f:(fun writer ->
            Io.Writer.add_string writer (String.make len '1');
            Io.Writer.flush writer)
      in
      Io.close output;
      printfn "writer: finished"
    in
    let read () =
      let+ contents = Io.with_read input ~f:Io.Reader.to_string in
      let len' = String.length contents in
      printfn "read: %d length" len';
      assert (len = len');
      Io.close input
    in
    Fiber.fork_and_join_unit read write
  in
  let print_errors f () =
    Fiber.with_error_handler
      ~on_error:(fun exn ->
        Format.eprintf "%a@." Exn_with_backtrace.pp_uncaught exn;
        Exn_with_backtrace.reraise exn)
      f
  in
  Lev_fiber.run (Lev.Loop.create ()) ~f:(print_errors run);
  [%expect {|
    writer: finished
    read: 6120 length |}]

let%expect_test "blocking pipe" =
  let fdr, fdw = Unix.pipe ~cloexec:true () in
  let run () =
    let* r = Io.create fdr `Blocking Input in
    let* w = Io.create fdw `Blocking Output in
    let writer () =
      let+ () =
        Io.with_write w ~f:(fun writer ->
            Io.Writer.add_string writer "foo bar baz";
            Io.Writer.flush writer)
      in
      printfn "writer: finished";
      Io.close w
    in
    let reader () =
      let+ read = Io.with_read r ~f:Io.Reader.to_string in
      printfn "read: %S" read;
      Io.close r
    in
    Fiber.fork_and_join_unit reader writer
  in
  Lev_fiber.run (Lev.Loop.create ()) ~f:run;
  [%expect {|
    writer: finished
    read: "foo bar baz" |}]
