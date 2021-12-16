open! Stdune
open Fiber.O
open Lev_fiber

let%expect_test "pipe" =
  let run () =
    let* input, output = Io.pipe ~cloexec:true () in
    let write () =
      let f = Faraday.create 100 in
      Faraday.write_string f "foobar";
      Faraday.close f;
      let+ res = Io.write output f in
      printfn "writer: finished";
      match res with `Yield -> assert false | `Close -> Io.close output
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
    reader: foobar |}]
