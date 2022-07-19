let%expect_test "toplevel exception" =
  (try
     ( Lev_fiber.run @@ fun () ->
       print_endline "raising Exit";
       let _ = raise Exit in
       Fiber.return () );
     assert false
   with Exit -> print_endline "caught Exit");
  [%expect {|
    raising Exit
    caught Exit |}]

let%expect_test "" =
  (try
     Lev_fiber.run @@ fun () ->
     Fiber.fork_and_join_unit
       (fun () ->
         print_endline "t1: raising";
         raise Exit)
       (fun () ->
         print_endline "t2: running";
         Fiber.return ())
   with Exit -> print_endline "caught Exit");
  [%expect {|
    t1: raising
    caught Exit |}]
