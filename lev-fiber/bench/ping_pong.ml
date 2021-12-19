open! Stdune
open Fiber.O
open Lev_fiber

let response = Bytes.of_string "+PONG\r\n"

let pong o times =
  let f = Faraday.create (Bytes.length response) in
  for _ = 1 to times do
    Faraday.write_bytes f response
  done;
  Faraday.close f;
  let+ w = Io.write o f in
  match w with `Yield -> assert false | `Close -> ()

let rec process_bytes buf pos len count =
  if pos >= len then count
  else
    let c = Bytes.get buf pos in
    let count = if c = '\n' then count + 1 else count in
    process_bytes buf (pos + 1) len count

let rec read o reader =
  match Io.Reader.available reader with
  | `Eof -> Fiber.return ()
  | `Ok 0 ->
      let* () = Io.Reader.refill reader in
      read o reader
  | `Ok _ ->
      let buf = Io.Reader.buffer reader in
      let len = Bytes.length buf in
      let times = process_bytes buf 0 len 0 in
      Io.Reader.consume reader ~len;
      let* () = pong o times in
      read o reader

let serve fd _sockaddr =
  Unix.set_nonblock fd;
  let* i, o = Io.create_rw fd `Non_blocking in
  let+ () = Io.with_read i ~f:(fun reader -> read o reader) in
  Io.close i;
  Io.close o

let run sock_path =
  let delete () = try Unix.unlink sock_path with Unix.Unix_error _ -> () in
  delete ();
  let socket = Unix.socket ~cloexec:true Unix.PF_UNIX Unix.SOCK_STREAM 0 in
  let addr = Unix.ADDR_UNIX sock_path in
  let* server = Socket.Server.create ~backlog:128 socket addr in
  at_exit delete;
  let serve fd s =
    Fiber.with_error_handler
      (fun () -> serve fd s)
      ~on_error:(fun exn ->
        Format.eprintf "%a@.%!" Exn_with_backtrace.pp_uncaught exn;
        Exn_with_backtrace.reraise exn)
  in
  Socket.Server.serve server ~f:serve

let () = Lev_fiber.run (Lev.Loop.create ()) ~f:(fun () -> run Sys.argv.(1))
