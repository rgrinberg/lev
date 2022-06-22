open Lev

let%expect_test "signal handling" =
  let signal = Sys.sigusr1 in
  let flags = Loop.Flag.(Set.singleton Nosigmask) in
  let loop = Loop.create ~flags () in
  let signal_watcher =
    Signal.create ~signal (fun t ->
        print_endline "received signal";
        ignore (Unix.sigprocmask Unix.SIG_BLOCK [ signal ]);
        Signal.stop t loop)
  in
  let idle =
    Idle.create (fun idle ->
        print_endline "sending signal";
        Unix.kill (Unix.getpid ()) signal;
        Idle.stop idle loop)
  in
  Signal.start signal_watcher loop;
  Idle.start idle loop;
  Loop.run_until_done loop;
  [%expect {|
    sending signal
    received signal |}]
