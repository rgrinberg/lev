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
      | Running r ->
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
          loop ()
  end
end

let wait ~pid =
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
    | Open of { buffer : Buffer.t; fd : Unix.file_descr; kind : kind }

  type 'a t = 'a state ref

  let create (type a) buffer fd kind (_ : a mode) : a t Fiber.t =
    let+ kind =
      match kind with
      | `Blocking ->
          let+ thread = Thread.create () in
          Blocking thread
      | `Non_blocking ->
          let events = assert false in
          let io = Lev.Io.create (fun _ _ _ -> assert false) fd events in
          Fiber.return (Non_blocking io)
    in
    ref (Open { buffer; fd; kind })

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
  module Client = struct
    type t

    let create _ = assert false

    let connect _ = assert false

    let stop _ = assert false
  end

  module Server = struct
    type t

    let create _ = assert false

    let stop _ = assert false

    let serve _ = assert false

    let listening_address _ = assert false
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
  let module Scheduler = Fiber.Scheduler in
  let rec loop = function Scheduler.Done a -> a | Stalled a -> stalled a
  and stalled a =
    let res = Lev.Loop.run lev_loop Once in
    let events = events queue [] in
    match Nonempty_list.of_list events with
    | None -> continue a res
    | Some fills -> loop (Scheduler.advance a fills)
  and events q acc =
    match Queue.pop q with None -> acc | Some e -> events q (e :: acc)
  and continue next res =
    match res with
    | `No_more_active_watchers -> Code_error.raise "deadlock" []
    | `Otherwise -> stalled next
  in
  let step = Scheduler.start f in
  loop step
