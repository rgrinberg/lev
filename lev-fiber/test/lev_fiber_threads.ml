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
  [%expect{| in thread |}]
