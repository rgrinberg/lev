open Printf
open Lev

let%expect_test "version" =
  let major, minor = ev_version () in
  printf "version (%d, %d)\n" major minor;
  [%expect {| version (4, 33) |}]

let%expect_test "default" =
  ignore (Loop.default ());
  [%expect {||}]

let%expect_test "create and run" =
  let ev = Loop.create () in
  (match Loop.run ev `No_wait with
  | `No_more_active_watchers -> ()
  | `Otherwise -> assert false);
  [%expect {||}]

let%expect_test "read from pipe" =
  let r, w = Unix.pipe () in
  Unix.set_nonblock r;
  Unix.set_nonblock w;
  let loop = Loop.create () in
  let io_r =
    Io.create
      (fun io loop _ ->
        let b = Bytes.make 1 '0' in
        match Unix.read r b 0 1 with
        | exception Unix.Unix_error (EAGAIN, _, _) -> ()
        | s ->
            assert (s = 1);
            printf "read char %s\n" (Bytes.to_string b);
            Unix.close r;
            Io.stop io loop)
      r
      (Io.Event.Set.create ~read:true ())
  in
  let io_w =
    Io.create
      (fun io loop _ ->
        ignore (Unix.write w (Bytes.make 1 'c') 0 1);
        print_endline "written to pipe";
        Unix.close w;
        Io.stop io loop)
      w
      (Io.Event.Set.create ~write:true ())
  in
  Io.start io_r loop;
  Io.start io_w loop;
  ignore (Lev.Loop.run loop `No_wait);
  [%expect {|
    written to pipe
    read char c |}]
