open Stdune
open Fiber.O
open Util
module Timestamp = Lev.Timestamp

type t = {
  loop : Lev.Loop.t;
  queue : Fiber.fill Queue.t;
  (* TODO stop when there are no threads *)
  async : Lev.Async.t;
  thread_jobs : Fiber.fill Queue.t;
  thread_mutex : Mutex.t;
}

type scheduler = t

let t : t Fiber.Var.t = Fiber.Var.create ()
let scheduler = t

module Buffer = struct
  include Bip_buffer

  type t = Bytes.t Bip_buffer.t

  let create ~size = create (Bytes.create size) ~len:size
end

module Thread = struct
  type job =
    | Job :
        (unit -> 'a)
        * ('a, [ `Exn of Exn_with_backtrace.t | `Cancelled ]) result
          Fiber.Ivar.t
        -> job

  type t = { worker : job Worker.t }

  let spawn_thread f =
    let (_ : Thread.t) = Thread.create f () in
    ()

  let create () =
    let+ t = Fiber.Var.get_exn t in
    let do_no_raise (Job (f, ivar)) =
      let res =
        match Exn_with_backtrace.try_with f with
        | Ok x -> Ok x
        | Error exn -> Error (`Exn exn)
      in
      Mutex.lock t.thread_mutex;
      Queue.push t.thread_jobs (Fiber.Fill (ivar, res));
      Mutex.unlock t.thread_mutex;
      Lev.Async.send t.async t.loop
    in
    let worker = Worker.create ~spawn_thread ~do_no_raise in
    { worker }

  type 'a task = {
    ivar :
      ('a, [ `Exn of Exn_with_backtrace.t | `Cancelled ]) result Fiber.Ivar.t;
    task : Worker.task;
  }

  let task t ~f =
    Fiber.of_thunk (fun () ->
        let ivar = Fiber.Ivar.create () in
        let task =
          match Worker.add_work t.worker (Job (f, ivar)) with
          | Ok task -> task
          | Error `Stopped -> Code_error.raise "already stopped" []
        in
        Fiber.return { ivar; task })

  let await task = Fiber.Ivar.read task.ivar

  let cancel task =
    let* status = Fiber.Ivar.peek task.ivar in
    match status with
    | Some _ -> Fiber.return ()
    | None ->
        Worker.cancel_if_not_consumed task.task;
        Fiber.Ivar.fill task.ivar (Error `Cancelled)

  let close t = Worker.complete_tasks_and_stop t.worker
end

module Timer = struct
  let sleepf after =
    let* t = Fiber.Var.get_exn t in
    let ivar = Fiber.Ivar.create () in
    let timer =
      Lev.Timer.create ~after (fun timer ->
          Lev.Timer.stop timer t.loop;
          Lev.Timer.destroy timer;
          Queue.push t.queue (Fiber.Fill (ivar, ())))
    in
    Lev.Timer.start timer t.loop;
    Fiber.Ivar.read ivar

  module Wheel = struct
    type elt = {
      ivar : [ `Ok | `Cancelled ] Fiber.Ivar.t;
      scheduled : Lev.Timestamp.t;
      mutable filled : bool;
      wheel : running;
    }

    and running = {
      queue : elt Removable_queue.t;
      delay : float;
      scheduler : scheduler;
      mutable waiting_filled : bool;
      mutable waiting : unit Fiber.Ivar.t option;
    }

    and state = Stopped | Running of running
    and t = state ref

    let create ~delay =
      let+ scheduler = Fiber.Var.get_exn t in
      ref
        (Running
           {
             queue = Removable_queue.create ();
             delay;
             scheduler;
             waiting_filled = false;
             waiting = None;
           })

    type task = elt Removable_queue.node ref

    let task (t : t) : task Fiber.t =
      match !t with
      | Stopped -> Code_error.raise "Wheel.task" []
      | Running t ->
          let now = Lev.Loop.now t.scheduler.loop in
          let data =
            {
              wheel = t;
              ivar = Fiber.Ivar.create ();
              scheduled = now;
              filled = false;
            }
          in
          let res = Removable_queue.push t.queue data in
          let+ () =
            match t.waiting with
            | None -> Fiber.return ()
            | Some ivar ->
                if t.waiting_filled then Fiber.return ()
                else (
                  t.waiting_filled <- true;
                  Fiber.Ivar.fill ivar ())
          in
          ref res

    let reset (task : task) =
      let task' = Removable_queue.data !task in
      if not task'.filled then (
        Removable_queue.remove !task;
        let now = Lev.Loop.now task'.wheel.scheduler.loop in
        let task' = { task' with scheduled = now } in
        let new_task = Removable_queue.push task'.wheel.queue task' in
        task := new_task)

    let await (task : task) =
      let task = Removable_queue.data !task in
      Fiber.Ivar.read task.ivar

    let cancel (node : task) =
      let task = Removable_queue.data !node in
      if task.filled then Fiber.return ()
      else (
        task.filled <- true;
        Removable_queue.remove !node;
        Fiber.Ivar.fill task.ivar `Cancelled)

    let rec run t =
      match !t with
      | Stopped -> Fiber.return ()
      | Running r -> (
          match Removable_queue.pop r.queue with
          | None ->
              let ivar = Fiber.Ivar.create () in
              r.waiting <- Some ivar;
              r.waiting_filled <- false;
              let* () = Fiber.Ivar.read ivar in
              r.waiting <- None;
              r.waiting_filled <- false;
              run t
          | Some task ->
              let after =
                let now = Timestamp.to_float (Lev.Loop.now r.scheduler.loop) in
                let scheduled = Timestamp.to_float task.scheduled in
                scheduled -. now +. r.delay
              in
              let scheduler = task.wheel.scheduler in
              let ivar = Fiber.Ivar.create () in
              let timer =
                Lev.Timer.create ~after (fun timer ->
                    (* TODO reuse timer *)
                    Lev.Timer.destroy timer;
                    Queue.push scheduler.queue (Fiber.Fill (ivar, ())))
              in
              Lev.Timer.start timer scheduler.loop;
              let* () = Fiber.Ivar.read ivar in
              let () =
                if not task.filled then (
                  task.filled <- true;
                  Queue.push scheduler.queue (Fiber.Fill (task.ivar, `Ok)))
              in
              run t)

    let stop t =
      match !t with
      | Stopped -> Fiber.return ()
      | Running r -> (
          t := Stopped;
          let rec loop () =
            match Removable_queue.pop r.queue with
            | None -> Fiber.return ()
            | Some task ->
                let* () =
                  if task.filled then Fiber.return ()
                  else (
                    task.filled <- true;
                    Fiber.Ivar.fill task.ivar `Cancelled)
                in
                loop ()
          in
          let* () = loop () in
          match r.waiting with
          | None -> Fiber.return ()
          | Some w -> Fiber.Ivar.fill w ())
  end
end

let waitpid ~pid =
  let* { loop; queue; _ } = Fiber.Var.get_exn t in
  let ivar = Fiber.Ivar.create () in
  let child =
    Lev.Child.create
      (fun t ~pid:_ process_status ->
        Queue.push queue (Fiber.Fill (ivar, process_status));
        Lev.Child.stop t loop;
        Lev.Child.destroy t)
      (Pid pid) Terminate
  in
  Lev.Child.start child loop;
  Fiber.Ivar.read ivar

module Io = struct
  type input
  type output
  type 'a mode = Input : input mode | Output : output mode
  type kind = Blocking of Thread.t | Non_blocking of Lev.Io.t

  type 'a state =
    | Closed
    | Open of {
        closer : unit Lazy.t;
        buffer : Buffer.t;
        fd : Unix.file_descr;
        kind : kind;
      }

  type 'a t = 'a state ref

  let create_gen (type a) fd buffer kind (mode : a mode) ~closer : a t Fiber.t =
    let+ kind =
      match kind with
      | `Blocking ->
          let+ thread = Thread.create () in
          Blocking thread
      | `Non_blocking ->
          let events =
            let read, write =
              match mode with Input -> (true, false) | Output -> (false, true)
            in
            Lev.Io.Event.Set.create ~read ~write ()
          in
          let io = Lev.Io.create (fun _ _ _ -> assert false) fd events in
          Fiber.return (Non_blocking io)
    in
    ref (Open { buffer; fd; kind; closer })

  let create fd buffer kind mode =
    create_gen ~closer:(lazy (Unix.close fd)) fd buffer kind mode

  let create_rw fd ~input ~output kind =
    let closer = lazy (Unix.close fd) in
    let* r = create_gen ~closer fd input kind Input in
    let+ w = create_gen ~closer fd output kind Output in
    (r, w)

  let flush _ = assert false

  module Slice = struct
    type t

    let length _ = assert false
    let get _ = assert false
    let sub _ = assert false
    let consume _ = assert false
  end

  type reader

  let read ?max _ =
    ignore max;
    assert false

  let with_read _ = assert false
  let closed _ = assert false

  let close t =
    match !t with
    | Closed -> ()
    | Open o ->
        Unix.close o.fd;
        t := Closed

  let fd _ = assert false
  let pipe () = assert false
end

module Socket = struct
  let connect fd sock =
    (* TODO windoze *)
    let* scheduler = Fiber.Var.get_exn scheduler in
    Unix.set_nonblock fd;
    let ivar = Fiber.Ivar.create () in
    match Unix.connect fd sock with
    | () -> Fiber.return ()
    | exception Unix.Unix_error (Unix.EINPROGRESS, _, _) -> (
        let io =
          Lev.Io.create
            (fun io _ _ ->
              Queue.push scheduler.queue (Fiber.Fill (ivar, ()));
              Lev.Io.stop io scheduler.loop;
              Lev.Io.destroy io)
            fd
            (Lev.Io.Event.Set.create ~write:true ())
        in
        Lev.Io.start io scheduler.loop;
        let+ () = Fiber.Ivar.read ivar in
        match Unix.getsockopt_error fd with
        | None -> ()
        | Some err -> raise (Unix.Unix_error (err, "connect", "")))

  module Server = struct
    type t = {
      fd : Unix.file_descr;
      pool : Fiber.Pool.t;
      io : Lev.Io.t;
      mutable close : bool;
      mutable await : unit Fiber.Ivar.t;
    }

    let create fd sockaddr ~backlog =
      let+ scheduler = Fiber.Var.get_exn scheduler in
      let pool = Fiber.Pool.create () in
      Unix.set_nonblock fd;
      Unix.bind fd sockaddr;
      Unix.listen fd backlog;
      let t = Fdecl.create Dyn.opaque in
      let io =
        Lev.Io.create
          (fun _ _ _ ->
            let t = Fdecl.get t in
            Queue.push scheduler.queue (Fiber.Fill (t.await, ())))
          fd
          (Lev.Io.Event.Set.create ~read:true ())
      in
      Fdecl.set t { pool; await = Fiber.Ivar.create (); close = false; fd; io };
      Fdecl.get t

    let close t =
      if t.close then Fiber.return ()
      else
        let* scheduler = Fiber.Var.get_exn scheduler in
        Unix.close t.fd;
        Lev.Io.stop t.io scheduler.loop;
        Lev.Io.destroy t.io;
        t.close <- true;
        let* () = Fiber.Pool.stop t.pool in
        Fiber.Ivar.fill t.await ()

    let serve (t : t) ~f =
      let* scheduler = Fiber.Var.get_exn scheduler in
      Lev.Io.start t.io scheduler.loop;
      Fiber.fork_and_join_unit
        (fun () -> Fiber.Pool.run t.pool)
        (fun () ->
          let rec loop () =
            let* () = Fiber.Ivar.read t.await in
            match t.close with
            | true -> Fiber.return ()
            | false ->
                t.await <- Fiber.Ivar.create ();
                let fd, sockaddr = Unix.accept ~cloexec:true t.fd in
                let* () = Fiber.Pool.task t.pool ~f:(fun () -> f fd sockaddr) in
                loop ()
          in
          loop ())
  end
end

let run lev_loop ~f =
  let thread_jobs = Queue.create () in
  let thread_mutex = Mutex.create () in
  let queue = Queue.create () in
  let async =
    Lev.Async.create (fun _ ->
        Mutex.lock thread_mutex;
        Queue.transfer thread_jobs queue;
        Mutex.unlock thread_mutex)
  in
  Lev.Async.start async lev_loop;
  let f =
    Fiber.Var.set t
      { loop = lev_loop; queue; async; thread_mutex; thread_jobs }
      f
  in
  let rec events q acc =
    match Queue.pop q with None -> acc | Some e -> events q (e :: acc)
  in
  let rec iter_or_deadlock q =
    match Nonempty_list.of_list (events q []) with
    | Some e -> e
    | None -> Code_error.raise "deadlock" []
  and iter loop q =
    match Nonempty_list.of_list (events q []) with
    | Some e -> e
    | None -> (
        let res = Lev.Loop.run loop Once in
        match res with
        | `No_more_active_watchers -> iter_or_deadlock q
        | `Otherwise -> iter loop q)
  in
  Fiber.run f ~iter:(fun () -> iter lev_loop queue)
