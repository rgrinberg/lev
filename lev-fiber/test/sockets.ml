open! Stdune
open Fiber.O
open Lev_fiber

let%expect_test "server" =
  let path = "levfiber.sock" in
  (try Unix.unlink path with Unix.Unix_error _ -> ());
  let sockaddr = Unix.ADDR_UNIX path in
  let domain = Unix.domain_of_sockaddr sockaddr in
  let socket () = Unix.socket ~cloexec:true domain Unix.SOCK_STREAM 0 in
  let run () =
    let ready_client = Fiber.Ivar.create () in
    let server () =
      print_endline "server: starting";
      let fd = socket () in
      Unix.setsockopt fd Unix.SO_REUSEADDR true;
      let* server = Socket.Server.create fd sockaddr ~backlog:10 in
      print_endline "server: created";
      let* () = Fiber.Ivar.fill ready_client () in
      let+ () =
        print_endline "server: serving";
        Socket.Server.serve server ~f:(fun fd _ ->
            print_endline "server: client connected";
            Unix.close fd;
            Socket.Server.close server)
      in
      print_endline "server: finished"
    in
    let client () =
      let* () = Fiber.Ivar.read ready_client in
      let fd = socket () in
      let* thread = Thread.create () in
      let* task =
        Thread.task thread ~f:(fun () ->
            Unix.set_nonblock fd;
            print_endline "client: connecting";
            Unix.connect fd sockaddr;
            print_endline "client: connected";
            Unix.close fd)
      in
      let+ res = Thread.await task in
      (match res with
      | Error `Cancelled -> assert false
      | Error (`Exn exn) ->
          Format.eprintf "%a@." Exn_with_backtrace.pp_uncaught exn
      | Ok () -> ());
      Thread.close thread
    in
    Fiber.fork_and_join_unit client server
  in
  Lev_fiber.run (Lev.Loop.create ()) ~f:run;
  [%expect
    {|
    server: starting
    server: created
    server: serving
    client: connecting
    client: connected
    server: client connected
    server: finished |}]
