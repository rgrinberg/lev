open! Stdune
open Fiber.O
open Lev_fiber

let%expect_test "pipe" =
  let run () =
    let* input, output = Io.pipe ~cloexec:true () in
    let write () =
      let+ () =
        Io.with_write output ~f:(fun writer ->
            let () =
              let str = "foobar" in
              let len = String.length str in
              let dst, { Io.Slice.pos; len = _ } =
                Io.Writer.prepare writer ~len
              in
              Bytes.blit_string ~src:str ~src_pos:0 ~dst ~dst_pos:pos ~len;
              Io.Writer.commit writer ~len
            in
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

let%expect_test "blocking pipe" =
  let fdr, fdw = Unix.pipe ~cloexec:true () in
  let run () =
    let* r = Io.create fdr `Blocking Input in
    let* w = Io.create fdw `Blocking Output in
    let writer () =
      let+ () =
        Io.with_write w ~f:(fun writer ->
            let src = "foo bar baz" in
            let len = String.length src in
            let dst, { Io.Slice.pos = dst_pos; len = _ } =
              Io.Writer.prepare writer ~len
            in
            Bytes.blit_string ~src ~src_pos:0 ~dst ~dst_pos ~len;
            Io.Writer.commit writer ~len;
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
