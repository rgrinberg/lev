open! Stdune
open Fiber.O
open Lev_fiber

let%expect_test "pipe" =
  let run () =
    let input, output = Io.pipe () in
    let write () =
      let f = Faraday.create 100 in
      Faraday.write_string f "foobar";
      let* res = Io.write output f in
      match res with `Yield -> assert false | `Close -> Fiber.return ()
    in
    let read () =
      let+ () =
        Io.with_read input ~f:(fun reader ->
            let rec loop buf =
              let* read = Io.read reader in
              match read with
              | None ->
                  let contents = Stdlib.Buffer.contents buf in
                  printfn "reader: %s" contents;
                  Fiber.return ()
              | Some slice ->
                  let b = Io.Slice.buffer slice in
                  Buffer.add_string buf (Bigstringaf.to_string b);
                  Io.Slice.consume slice (Bigstringaf.length b);
                  loop buf
            in
            loop (Buffer.create 15))
      in
      Io.close input
    in
    Fiber.fork_and_join_unit read write
  in
  Lev_fiber.run (Lev.Loop.create ()) ~f:run;
  [%expect {||}]
