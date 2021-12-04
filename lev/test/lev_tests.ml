open Printf
open Lev
module List = ListLabels

let%expect_test "version" =
  let major, minor = ev_version () in
  printf "version (%d, %d)\n" major minor;
  [%expect {| version (4, 33) |}]

let%expect_test "default" =
  ignore (Loop.default ());
  [%expect {||}]

let%expect_test "now" =
  let loop = Loop.default () in
  let (_ : Timestamp.t) = Loop.now loop in
  [%expect {||}]

let%expect_test "sleep" =
  Timestamp.sleep (Timestamp.of_float 0.1);
  [%expect {||}]

let%expect_test "suspend/resume" =
  let loop = Loop.create () in
  Loop.suspend loop;
  Loop.resume loop;
  [%expect {||}]

let%expect_test "create and run" =
  let ev = Loop.create () in
  (match Loop.run ev Nowait with
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
      (fun io fd events ->
        let b = Bytes.make 1 '0' in
        match Unix.read fd b 0 1 with
        | exception Unix.Unix_error (EAGAIN, _, _) -> ()
        | s ->
            assert (Io.Event.Set.mem events Read);
            assert (s = 1);
            printf "read char %s\n" (Bytes.to_string b);
            Unix.close r;
            Io.stop io loop)
      r
      (Io.Event.Set.create ~read:true ())
  in
  let io_w =
    Io.create
      (fun io fd events ->
        assert (Io.Event.Set.mem events Write);
        ignore (Unix.write fd (Bytes.make 1 'c') 0 1);
        print_endline "written to pipe";
        Unix.close w;
        Io.stop io loop)
      w
      (Io.Event.Set.create ~write:true ())
  in
  Io.start io_r loop;
  Io.start io_w loop;
  ignore (Loop.run_until_done loop);
  [%expect {|
    written to pipe
    read char c |}]

let%expect_test "timer" =
  let loop = Loop.create () in
  let timer =
    Timer.create ~after:0.02 (fun timer ->
        print_endline "fired timer";
        Timer.stop timer loop)
  in
  Timer.start timer loop;
  ignore (Lev.Loop.run loop Once);
  [%expect {|
    fired timer |}]

let%expect_test "periodic timer" =
  let loop = Loop.create () in
  let timer =
    let count = ref 3 in
    Timer.create ~after:0. ~repeat:0.02 (fun timer ->
        if !count = 0 then
          let () = print_endline "stopping timer" in
          Timer.stop timer loop
        else (
          decr count;
          print_endline "fired timer"))
  in
  Timer.start timer loop;
  ignore (Lev.Loop.run_until_done loop);
  [%expect
    {|
    fired timer
    fired timer
    fired timer
    stopping timer |}]

let%expect_test "cleanup callbacks" =
  let loop = Loop.create () in
  let cleanup = Cleanup.create (fun _ -> print_endline "cleanup") in
  Cleanup.start cleanup loop;
  ignore (Loop.run loop Nowait);
  Loop.destroy loop;
  [%expect {| cleanup |}]

let%expect_test "child" =
  let loop = Loop.default () in
  let stdin, stdin_w = Unix.pipe ~cloexec:true () in
  let stdout_r, stdout = Unix.pipe ~cloexec:true () in
  let stderr_r, stderr = Unix.pipe ~cloexec:true () in
  Unix.close stdin_w;
  Unix.close stdout_r;
  Unix.close stderr_r;
  let pid =
    Unix.create_process "sh" [| "sh"; "-c"; "exit 42" |] stdin stdout stderr
  in
  let child =
    Child.create
      (fun t ~pid:pid' status ->
        Child.stop t loop;
        (match status with
        | Unix.WEXITED i -> printf "exited with status %d\n" i
        | _ -> assert false);
        assert (pid = pid'))
      (Pid pid) Terminate
  in
  Child.start child loop;
  Loop.run_until_done loop;
  [%expect {| exited with status 42 |}]

let%expect_test "periodic - regular" =
  let loop = Loop.create () in
  let periodic =
    Periodic.create
      (fun p ->
        print_endline "periodic fired";
        Periodic.stop p loop)
      (Regular { offset = Timestamp.of_float 0.1; interval = None })
  in
  Periodic.start periodic loop;
  Loop.run_until_done loop;
  [%expect {| periodic fired |}]

let%expect_test "periodic - custom" =
  let loop = Loop.create () in
  let periodic =
    Periodic.create
      (fun p ->
        print_endline "periodic fired";
        Periodic.stop p loop)
      (Custom
         (fun _ ~now ->
           let now = Timestamp.to_float now in
           Timestamp.of_float (now +. 0.2)))
  in
  Periodic.start periodic loop;
  Loop.run_until_done loop;
  [%expect {| periodic fired |}]

let%expect_test "check/idle/prepare" =
  let loop = Loop.create () in
  let check = Check.create (fun _ -> print_endline "check") in
  let idle = Idle.create (fun _ -> print_endline "idle") in
  let prepare = Prepare.create (fun _ -> print_endline "prepare") in
  [ Check.start check; Idle.start idle; Prepare.start prepare ]
  |> List.iter ~f:(fun f -> f loop);
  ignore (Loop.run loop Once);
  [%expect {|
    prepare
    check
    idle |}]

let%expect_test "async" =
  let loop = Loop.create () in
  let async = Async.create (fun _ -> print_endline "async fired") in
  let prepare =
    Prepare.create (fun _ ->
        print_endline "firing async";
        Async.send async loop)
  in
  Prepare.start prepare loop;
  Async.start async loop;
  ignore (Loop.run loop Once);
  [%expect {|
    firing async
    async fired |}]

let%expect_test "is_pending/is_active" =
  let loop = Loop.create () in
  let idle = Idle.create (fun _ -> print_endline "idle") in
  let print_status () =
    printf "pending = %b; active = %b\n" (Idle.is_pending idle)
      (Idle.is_active idle)
  in
  print_status ();
  Idle.start idle loop;
  print_status ();
  ignore (Loop.run loop Once);
  print_status ();
  Idle.stop idle loop;
  print_status ();
  [%expect
    {|
    pending = false; active = false
    pending = false; active = true
    idle
    pending = false; active = true
    pending = false; active = false |}]

let%expect_test "destroy" =
  let loop = Loop.create () in
  let idle =
    Idle.create (fun idle ->
        print_endline "idle";
        Idle.stop idle loop)
  in
  Idle.start idle loop;
  ignore (Loop.run loop Once);
  assert (not (Idle.is_active idle));
  assert (not (Idle.is_pending idle));
  print_endline "destroying watcher";
  Idle.destroy idle;
  [%expect {|
    idle
    destroying watcher |}]

let%expect_test "timer - stops automatically" =
  let loop = Loop.create () in
  let another = ref true in
  let timer =
    Timer.create ~after:0.01 (fun t ->
        print_endline "timer fired";
        if !another then (
          another := false;
          Timer.start t loop))
  in
  Timer.start timer loop;
  Loop.run_until_done loop;
  [%expect {|
    timer fired
    timer fired |}]

let%expect_test "timer/again cancels start" =
  let loop = Loop.create () in
  let timer =
    Timer.create ~after:0.05 (fun t ->
        print_endline "timer fired";
        Timer.stop t loop)
  in
  Timer.start timer loop;
  Timer.again timer loop;
  Loop.run_until_done loop;
  [%expect {| |}]

let%expect_test "timer/consecutive again" =
  let loop = Loop.create () in
  let now = Unix.time () in
  let timer =
    Timer.create ~after:1.0 (fun t ->
        printf "timer fired after %f\n" (Unix.time () -. now);
        Timer.stop t loop)
  in
  let control =
    let count = ref 3 in
    Timer.create ~after:0.0 ~repeat:0.2 (fun t ->
        if !count = 0 then Timer.stop t loop
        else (
          decr count;
          print_endline "resetting timer";
          Timer.again timer loop))
  in
  Timer.start timer loop;
  Timer.start control loop;
  Loop.run_until_done loop;
  [%expect {|
    resetting timer
    resetting timer
    resetting timer |}]

exception Idle

let%expect_test "callback exception" =
  let loop = Loop.create () in
  let idle = Idle.create (fun _ -> raise Idle) in
  Idle.start idle loop;
  let with_idle () =
    try ignore (Loop.run loop Once) with Idle -> print_endline "caught idle!"
  in
  with_idle ();
  with_idle ();
  [%expect{|
    caught idle!
    caught idle! |}]
