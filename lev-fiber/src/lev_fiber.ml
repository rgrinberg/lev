open Stdune
open Fiber.O
open Lev_fiber_util
module Timestamp = Lev.Timestamp

type process = { pid : Pid.t; ivar : Unix.process_status Fiber.Ivar.t }

type process_watcher = {
  mutable process_thread : unit Lazy.t;
  active : (Pid.t, process) Table.t;
  mutex : Mutex.t;
}

type t = {
  loop : Lev.Loop.t;
  queue : Fiber.fill Queue.t;
  (* TODO stop when there are no threads *)
  async : Lev.Async.t;
  thread_jobs : Fiber.fill Queue.t;
  thread_mutex : Mutex.t;
  process_watcher : process_watcher option;
}

type scheduler = t

let t : t Fiber.Var.t = Fiber.Var.create ()
let scheduler = t

module Buffer = struct
  include Bip_buffer

  let default_size = 4096

  type nonrec t = bytes t

  let create ~size : t = create (Stdlib.Bytes.create size) ~len:size
end

module State = struct
  type 'a t' = Open of 'a | Closed
  type 'a t = 'a t' ref

  let check_open t =
    match !t with Closed -> Code_error.raise "must be opened" [] | Open a -> a

  let create a = ref (Open a)
  let close t = t := Closed
end

let finish_job t fill =
  Mutex.lock t.thread_mutex;
  Queue.push t.thread_jobs fill;
  Mutex.unlock t.thread_mutex;
  Lev.Async.send t.async t.loop

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
      finish_job t (Fiber.Fill (ivar, res))
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
    type running_state =
      | Idle
      | Sleeping of Lev.Timer.t * unit Fiber.Ivar.t
      (* set whenever the wheel is waiting for a new task *)
      | Waiting of { ivar : unit Fiber.Ivar.t; filled : bool }

    type elt = {
      ivar : [ `Ok | `Cancelled ] Fiber.Ivar.t;
      scheduled : Lev.Timestamp.t;
      mutable filled : bool;
      wheel : t;
    }

    and running = {
      queue : elt Removable_queue.t;
      delay : float;
      scheduler : scheduler;
      mutable state : running_state;
    }

    and state = Stopped of { delay : float } | Running of running
    and t = state ref

    let delay t =
      match !t with Stopped { delay } -> delay | Running { delay; _ } -> delay

    let create ~delay =
      let+ scheduler = Fiber.Var.get_exn t in
      ref
        (Running
           { queue = Removable_queue.create (); delay; scheduler; state = Idle })

    type task = elt Removable_queue.node ref
    type condition = { sleeping : bool; waiting : bool }

    let wakeup_if t { sleeping; waiting } =
      match t.state with
      | Sleeping (timer, ivar) when sleeping ->
          let* { loop; _ } = Fiber.Var.get_exn scheduler in
          Lev.Timer.stop timer loop;
          Lev.Timer.destroy timer;
          t.state <- Idle;
          Fiber.Ivar.fill ivar ()
      | Waiting { ivar; filled = false } when waiting ->
          t.state <- Idle;
          Fiber.Ivar.fill ivar ()
      | _ -> Fiber.return ()

    let set_delay t ~delay =
      match !t with
      | Stopped _ -> Code_error.raise "Wheel.set_delay" []
      | Running d ->
          t := Running { d with delay };
          wakeup_if d { sleeping = true; waiting = false }

    let task (t : t) : task Fiber.t =
      Fiber.of_thunk (fun () ->
          match !t with
          | Stopped _ -> Code_error.raise "Wheel.task" []
          | Running wheel ->
              let now = Lev.Loop.now wheel.scheduler.loop in
              let data =
                {
                  wheel = t;
                  ivar = Fiber.Ivar.create ();
                  scheduled = now;
                  filled = false;
                }
              in
              let res = Removable_queue.push wheel.queue data in
              let+ () = wakeup_if wheel { waiting = true; sleeping = false } in
              ref res)

    let reset (task : task) =
      Fiber.of_thunk (fun () ->
          let task' = Removable_queue.data !task in
          match !(task'.wheel) with
          | Stopped _ -> Code_error.raise "reset: wheel is stopped" []
          | Running wheel ->
              Removable_queue.remove !task;
              let now = Lev.Loop.now wheel.scheduler.loop in
              let filled = task'.filled in
              let task' =
                let task' = { task' with scheduled = now } in
                if filled then (
                  task'.filled <- false;
                  { task' with ivar = Fiber.Ivar.create () })
                else task'
              in
              let new_task = Removable_queue.push wheel.queue task' in
              task := new_task;
              if filled then
                wakeup_if wheel { sleeping = false; waiting = true }
              else Fiber.return ())

    let await (task : task) =
      Fiber.of_thunk (fun () ->
          let task = Removable_queue.data !task in
          Fiber.Ivar.read task.ivar)

    let cancel (node : task) =
      Fiber.of_thunk (fun () ->
          let task = Removable_queue.data !node in
          if task.filled then Fiber.return ()
          else (
            task.filled <- true;
            Removable_queue.remove !node;
            Fiber.Ivar.fill task.ivar `Cancelled))

    let rec run t =
      (* TODO do not allow double [run] *)
      match !t with
      | Stopped _ -> Fiber.return ()
      | Running r -> (
          match Removable_queue.peek r.queue with
          | None ->
              let ivar = Fiber.Ivar.create () in
              r.state <- Waiting { ivar; filled = false };
              let* () = Fiber.Ivar.read ivar in
              run t
          | Some node ->
              let task = Removable_queue.data node in
              let after =
                let now = Timestamp.to_float (Lev.Loop.now r.scheduler.loop) in
                let scheduled = Timestamp.to_float task.scheduled in
                scheduled -. now +. r.delay
              in
              let expired = after < 0. in
              let* () =
                if expired then (
                  Removable_queue.remove node;
                  if not task.filled then (
                    task.filled <- true;
                    Queue.push r.scheduler.queue (Fiber.Fill (task.ivar, `Ok)));
                  Fiber.return ())
                else
                  let scheduler = r.scheduler in
                  let ivar = Fiber.Ivar.create () in
                  let timer =
                    Lev.Timer.create ~after (fun timer ->
                        (* TODO reuse timer *)
                        Lev.Timer.destroy timer;
                        Queue.push scheduler.queue (Fiber.Fill (ivar, ())))
                  in
                  r.state <- Sleeping (timer, ivar);
                  Lev.Timer.start timer scheduler.loop;
                  Fiber.Ivar.read ivar
              in
              run t)

    let run t = Fiber.of_thunk (fun () -> run t)

    let stop =
      let rec cancel_all r =
        match Removable_queue.pop r.queue with
        | None -> Fiber.return ()
        | Some task ->
            let* () =
              if task.filled then Fiber.return ()
              else (
                task.filled <- true;
                Fiber.Ivar.fill task.ivar `Cancelled)
            in
            cancel_all r
      in
      fun t ->
        Fiber.of_thunk (fun () ->
            match !t with
            | Stopped _ -> Fiber.return ()
            | Running r ->
                t := Stopped { delay = r.delay };
                let* () = cancel_all r in
                wakeup_if r { sleeping = true; waiting = true })
  end
end

let waitpid_win32 ~pid =
  let* t = Fiber.Var.get_exn t in
  let pid = Pid.of_int pid in
  let watcher =
    match t.process_watcher with
    | None -> assert false
    | Some watcher ->
        Lazy.force watcher.process_thread;
        watcher
  in
  let ivar = Fiber.Ivar.create () in
  Mutex.lock watcher.mutex;
  Table.add_exn watcher.active pid { pid; ivar };
  Mutex.unlock watcher.mutex;
  Fiber.Ivar.read ivar

let waitpid_unix create ~pid =
  let* { loop; queue; _ } = Fiber.Var.get_exn t in
  let ivar = Fiber.Ivar.create () in
  let child =
    create
      (fun t ~pid:_ process_status ->
        Queue.push queue (Fiber.Fill (ivar, process_status));
        Lev.Child.stop t loop;
        Lev.Child.destroy t)
      (Lev.Child.Pid pid) Lev.Child.Terminate
  in
  Lev.Child.start child loop;
  Fiber.Ivar.read ivar

let waitpid =
  match Lev.Child.create with
  | Error `Unimplemented -> waitpid_win32
  | Ok create -> waitpid_unix create

let signal ~signal =
  let* { loop; queue; _ } = Fiber.Var.get_exn t in
  let ivar = Fiber.Ivar.create () in
  let signal =
    Lev.Signal.create ~signal (fun t ->
        Queue.push queue (Fiber.Fill (ivar, ()));
        Lev.Signal.stop t loop;
        Lev.Signal.destroy t)
  in
  Lev.Signal.start signal loop;
  Fiber.Ivar.read ivar

module Rc : sig
  type 'a t

  val get : 'a t -> 'a option
  val get_exn : 'a t -> 'a
  val release : _ t -> unit
  val create : count:int -> finalize:(unit -> unit) -> 'a -> 'a t
end = struct
  type 'a state = { data : 'a; mutable count : int; finalize : unit -> unit }
  type 'a t = 'a state State.t

  let get t = match !t with State.Closed -> None | Open a -> Some a.data
  let get_exn t = Option.value_exn (get t)
  let create ~count ~finalize data = State.create { count; finalize; data }

  let _retain t =
    let t = State.check_open t in
    t.count <- t.count + 1

  let release (t : _ t) =
    match !t with
    | Closed -> ()
    | Open a ->
        a.count <- a.count - 1;
        if a.count = 0 then (
          State.close t;
          a.finalize ())
end

module Fd = struct
  type kind = Blocking | Non_blocking of { mutable set : bool }
  type t = { fd : Unix.file_descr; kind : kind; mutable closed : bool }

  let fd t = t.fd

  let close t =
    if not t.closed then (
      t.closed <- true;
      Unix.close t.fd)

  let create' fd kind = { kind; fd; closed = false }

  let create fd kind =
    let kind =
      match kind with
      | `Blocking -> Blocking
      | `Non_blocking set -> Non_blocking { set }
    in
    create' fd kind

  let set_nonblock t =
    match t.kind with
    | Blocking -> ()
    | Non_blocking nb ->
        if not nb.set then (
          Unix.set_nonblock t.fd;
          nb.set <- true)

  let pipe =
    if Sys.win32 then fun ?cloexec () ->
      let r, w = Unix.pipe ?cloexec () in
      ( { fd = r; kind = Blocking; closed = false },
        { fd = w; kind = Blocking; closed = false } )
    else fun ?cloexec () ->
      let r, w = Unix.pipe ?cloexec () in
      Unix.set_nonblock r;
      Unix.set_nonblock w;
      ( { fd = r; kind = Non_blocking { set = true }; closed = false },
        { fd = w; kind = Non_blocking { set = true }; closed = false } )
end

module Lev_fd = struct
  module Event = Lev.Io.Event

  type t = {
    io : Lev.Io.t;
    fd : Fd.t;
    scheduler : scheduler;
    mutable events : Event.Set.t;
    read : unit Fiber.Ivar.t Queue.t;
    write : unit Fiber.Ivar.t Queue.t;
  }

  let reset nb new_set =
    nb.events <- new_set;
    Lev.Io.stop nb.io nb.scheduler.loop;
    Lev.Io.modify nb.io nb.events;
    Lev.Io.start nb.io nb.scheduler.loop

  let await t (what : Lev.Io.Event.t) =
    Fiber.of_thunk (fun () ->
        if not (Event.Set.mem t.events what) then
          reset t (Event.Set.add t.events what);
        let ivar = Fiber.Ivar.create () in
        let q = match what with Write -> t.write | Read -> t.read in
        Queue.push q ivar;
        Fiber.Ivar.read ivar)

  let make_finalizer loop io () =
    Lev.Io.stop io loop;
    let fd = Lev.Io.fd io in
    Unix.close fd;
    Lev.Io.destroy io

  let make_cb t scheduler _ _ set =
    match Rc.get (Fdecl.get t) with
    | None -> ()
    | Some nb ->
        let keep_read = ref true in
        let keep_write = ref true in
        (if Lev.Io.Event.Set.mem set Read then
         match Queue.pop nb.read with
         | Some ivar -> Queue.push scheduler.queue (Fiber.Fill (ivar, ()))
         | None -> keep_read := false);
        (if Lev.Io.Event.Set.mem set Write then
         match Queue.pop nb.write with
         | Some ivar -> Queue.push scheduler.queue (Fiber.Fill (ivar, ()))
         | None -> keep_write := false);
        let new_set =
          Event.Set.inter nb.events
            (Event.Set.create ~read:!keep_read ~write:!keep_write ())
        in
        if not (Event.Set.equal new_set nb.events) then reset nb new_set

  let create ~ref_count (fd : Fd.t) : t Rc.t Fiber.t =
    let+ scheduler = Fiber.Var.get_exn scheduler in
    let t : t Rc.t Fdecl.t = Fdecl.create Dyn.opaque in
    let events = Event.Set.create () in
    let io = Lev.Io.create (make_cb t scheduler) fd.fd events in
    Fdecl.set t
      (Rc.create
         ~finalize:(make_finalizer scheduler.loop io)
         ~count:ref_count
         {
           events;
           fd;
           scheduler;
           io;
           read = Queue.create ();
           write = Queue.create ();
         });
    Lev.Io.start io scheduler.loop;
    Fdecl.get t
end

module Io = struct
  type input = Input
  type output = Output
  type 'a mode = Input : input mode | Output : output mode

  module Slice = Buffer.Slice

  type _ kind =
    | Write : { mutable flush_counter : int } -> output kind
    | Read : { mutable eof : bool } -> input kind

  type fd = Blocking of Thread.t * Fd.t Rc.t | Non_blocking of Lev_fd.t Rc.t

  let with_ fd (kind : Lev.Io.Event.t) ~f =
    Fiber.of_thunk (fun () ->
        match fd with
        | Non_blocking lev_fd ->
            let lev_fd = Rc.get_exn lev_fd in
            let+ () = Lev_fd.await lev_fd kind in
            Result.try_with (fun () -> f lev_fd.fd)
        | Blocking (th, fd) -> (
            let fd = Rc.get_exn fd in
            let* task = Thread.task th ~f:(fun () -> f fd) in
            let+ res = Thread.await task in
            match res with
            | Ok _ as s -> s
            | Error `Cancelled -> assert false
            | Error (`Exn exn) -> Error exn.exn))

  let release t =
    match t with
    | Non_blocking fd -> Rc.release fd
    | Blocking (th, fd) ->
        Thread.close th;
        Rc.release fd

  type 'a open_ = { mutable buffer : Buffer.t; kind : 'a kind; fd : fd }
  type 'a t = 'a open_ State.t

  let fd (t : _ t) =
    match (State.check_open t).fd with
    | Blocking (_, fd) -> Rc.get_exn fd
    | Non_blocking fd -> (Rc.get_exn fd).fd

  let rec with_resize_buffer t ~len reserve_fail k =
    match Buffer.reserve t.buffer ~len with
    | Some dst_pos -> k t ~len ~dst_pos
    | None -> (
        match reserve_fail with
        | `Compress ->
            if Buffer.unused_space t.buffer >= len then
              Buffer.Bytes.compress t.buffer;
            with_resize_buffer t ~len `Resize k
        | `Resize ->
            let len = Buffer.length t.buffer + len in
            Buffer.Bytes.resize t.buffer ~len;
            with_resize_buffer t ~len `Fail k
        | `Fail -> assert false)

  module Writer = struct
    type nonrec t = output open_

    module Expert = struct
      let available t = Buffer.max_available t.buffer

      let prepare =
        let k t ~len ~dst_pos:pos =
          let buf = Buffer.buffer t.buffer in
          (buf, { Slice.pos; len })
        in
        fun t ~len -> with_resize_buffer t ~len `Compress k

      let commit t ~len = Buffer.commit t.buffer ~len
    end

    let flush =
      let rec loop t stop_count =
        (* TODO fix overflow issues *)
        if
          (match t.kind with Write { flush_counter } -> flush_counter)
          >= stop_count
        then Fiber.return ()
        else
          let buffer = Buffer.buffer t.buffer in
          let* res =
            with_ t.fd Write ~f:(fun fd ->
                match Buffer.peek t.buffer with
                | None -> ()
                | Some { Slice.pos; len } -> (
                    let len = Unix.single_write fd.fd buffer pos len in
                    Buffer.junk t.buffer ~len;
                    match t.kind with
                    | Write t -> t.flush_counter <- t.flush_counter + len))
          in
          match res with
          | Error (Unix.Unix_error (Unix.EAGAIN, _, _)) -> loop t stop_count
          | Error exn -> reraise exn
          | Ok () -> loop t stop_count
      in
      fun t ->
        Fiber.of_thunk (fun () ->
            let stop_count =
              match t.kind with
              | Write { flush_counter } ->
                  flush_counter + Buffer.length t.buffer
            in
            loop t stop_count)

    let add_substring t str ~pos ~len =
      Buffer.Bytes.Writer.add_substring t.buffer str ~pos ~len

    let add_string t str = Buffer.Bytes.Writer.add_string t.buffer str
  end

  let create_gen (type a) fd (mode : a mode) =
    let buffer = Buffer.create ~size:Buffer.default_size in
    let kind : a kind =
      match mode with
      | Input -> Read { eof = false }
      | Output -> Write { flush_counter = 0 }
    in
    State.create { buffer; fd; kind }

  let create (type a) (fd : Fd.t) (mode : a mode) =
    match fd.kind with
    | Non_blocking _ ->
        let+ fd = Lev_fd.create ~ref_count:1 fd in
        create_gen (Non_blocking fd) mode
    | Blocking ->
        let+ thread = Thread.create () in
        let fd = Rc.create ~count:1 ~finalize:(fun () -> Fd.close fd) fd in
        create_gen (Blocking (thread, fd)) mode

  let create_rw (fd : Fd.t) : (input t * output t) Fiber.t =
    match fd.kind with
    | Non_blocking _ ->
        let+ fd =
          let+ fd = Lev_fd.create ~ref_count:2 fd in
          Non_blocking fd
        in
        let r = create_gen fd Input in
        let w = create_gen fd Output in
        (r, w)
    | Blocking ->
        let fd = Rc.create ~count:2 ~finalize:(fun () -> Fd.close fd) fd in
        let* r =
          let+ thread = Thread.create () in
          create_gen (Blocking (thread, fd)) Input
        in
        let+ w =
          let+ thread = Thread.create () in
          create_gen (Blocking (thread, fd)) Output
        in
        (r, w)

  let close t =
    match !t with
    | State.Closed -> ()
    | Open o ->
        release o.fd;
        t := Closed

  module Reader = struct
    type t = input open_

    exception Unavailable

    module Expert = struct
      let buffer t =
        match Buffer.peek t.buffer with
        | None -> raise Unavailable
        | Some { Buffer.Slice.pos; len } ->
            let b = Buffer.buffer t.buffer in
            (b, { Slice.pos; len })

      let consume (t : t) ~len = Buffer.junk t.buffer ~len

      let available t =
        let eof = match t.kind with Read { eof } -> eof in
        let available = Buffer.length t.buffer in
        if available = 0 && eof then `Eof else `Ok available

      let refill =
        let rec read t ~len ~dst_pos =
          let b = Bytes.create len in
          let* res = with_ t.fd Read ~f:(fun fd -> Unix.read fd.fd b 0 len) in
          match res with
          | Error (Unix.Unix_error (Unix.EAGAIN, _, _)) -> read t ~len ~dst_pos
          | Ok 0 | Error (Unix.Unix_error (Unix.EBADF, _, _)) ->
              (match t.kind with Read b -> b.eof <- true);
              Buffer.commit t.buffer ~len:0;
              Fiber.return ()
          | Ok len ->
              Bytes.blit ~src:b ~src_pos:0 ~dst:(Buffer.buffer t.buffer)
                ~dst_pos ~len;
              Buffer.commit t.buffer ~len;
              Fiber.return ()
          | Error exn -> reraise exn
        in
        fun ?(size = Buffer.default_size) t ->
          with_resize_buffer t ~len:size `Compress read
    end

    exception Found of int

    let read_char_exn t =
      let b, { Buffer.Slice.pos; len = _ } = Expert.buffer t in
      let res = Bytes.get b pos in
      Expert.consume t ~len:1;
      res

    let read_line =
      let contents buf =
        let module Buffer = Stdlib.Buffer in
        let len = Buffer.length buf in
        if len = 0 then ""
        else if Buffer.nth buf (len - 1) = '\r' then Buffer.sub buf 0 (len - 1)
        else Buffer.contents buf
      in
      let find_nl b pos len =
        try
          for i = pos to pos + len - 1 do
            if Bytes.get b i = '\n' then raise_notrace (Found i)
          done;
          None
        with Found i -> Some i
      in
      let rec loop t buf =
        match Expert.available t with
        | `Eof ->
            Fiber.return (Error (`Partial_eof (Stdlib.Buffer.contents buf)))
        | `Ok 0 ->
            let* () = Expert.refill t in
            loop t buf
        | `Ok _ -> (
            let b, { Slice.pos; len } = Expert.buffer t in
            match find_nl b pos len with
            | Some i ->
                let len = i - pos in
                Stdlib.Buffer.add_subbytes buf b pos len;
                Buffer.junk t.buffer ~len:(len + 1);
                Fiber.return (Ok (contents buf))
            | None ->
                Stdlib.Buffer.add_subbytes buf b pos len;
                Buffer.junk t.buffer ~len;
                loop t buf)
      in
      let rec self t =
        (* we can always call loop, but we do a little optimization to see if we can
           read the line without an extra copy
        *)
        match Expert.available t with
        | `Eof -> Fiber.return (Error (`Partial_eof ""))
        | `Ok 0 ->
            let* () = Expert.refill t in
            self t
        | `Ok _ -> (
            let b, { Slice.pos; len } = Expert.buffer t in
            match find_nl b pos len with
            | Some i ->
                let len = i - pos in
                let res =
                  let len =
                    if len > 0 && Bytes.get b (i - 1) = '\r' then len - 1
                    else len
                  in
                  Bytes.sub b ~pos ~len
                in
                Buffer.junk t.buffer ~len:(len + 1);
                Fiber.return (Ok (Bytes.unsafe_to_string res))
            | None ->
                let buf = Stdlib.Buffer.create len in
                Stdlib.Buffer.add_subbytes buf b pos len;
                Buffer.junk t.buffer ~len;
                loop t buf)
      in
      fun t -> Fiber.of_thunk (fun () -> self t)

    let read_exactly =
      let rec loop_buffer t buf remains =
        if remains = 0 then Fiber.return (Ok (Stdlib.Buffer.contents buf))
        else
          match Expert.available t with
          | `Eof ->
              Fiber.return (Error (`Partial_eof (Stdlib.Buffer.contents buf)))
          | `Ok 0 ->
              let* () = Expert.refill t in
              loop_buffer t buf remains
          | `Ok _ ->
              let b, { Slice.pos; len } = Expert.buffer t in
              let len = min remains len in
              Stdlib.Buffer.add_subbytes buf b pos len;
              Buffer.junk t.buffer ~len;
              loop_buffer t buf (remains - len)
      in
      let rec self t len =
        (* we can always call loop, but we do a little optimization to see if we can
           read the line without an extra copy
        *)
        match Expert.available t with
        | `Eof -> Fiber.return (Error (`Partial_eof ""))
        | `Ok 0 ->
            let* () = Expert.refill t in
            self t len
        | `Ok _ ->
            let b, { Slice.pos; len = avail } = Expert.buffer t in
            if len <= avail then (
              let res = Bytes.sub b ~pos ~len in
              Buffer.junk t.buffer ~len;
              Fiber.return (Ok (Bytes.unsafe_to_string res)))
            else
              let buf = Stdlib.Buffer.create len in
              Stdlib.Buffer.add_subbytes buf b pos avail;
              Buffer.junk t.buffer ~len:avail;
              loop_buffer t buf (len - avail)
      in
      fun t len -> Fiber.of_thunk (fun () -> self t len)

    let to_string =
      let rec loop t buf =
        match Expert.available t with
        | `Eof -> Fiber.return (Stdlib.Buffer.contents buf)
        | `Ok 0 ->
            let* () = Expert.refill t in
            loop t buf
        | `Ok _ ->
            let b, { Slice.pos; len } = Expert.buffer t in
            Stdlib.Buffer.add_subbytes buf b pos len;
            Expert.consume t ~len;
            loop t buf
      in
      fun t -> Fiber.of_thunk (fun () -> loop t (Stdlib.Buffer.create 512))
  end

  let with_read (t : input t) ~f =
    let t = State.check_open t in
    f t

  let with_write (t : output t) ~f =
    let t = State.check_open t in
    f t

  let pipe ?cloexec () : (input t * output t) Fiber.t =
    Fiber.of_thunk @@ fun () ->
    let r, w = Fd.pipe ?cloexec () in
    let* input = create r Input in
    let+ output = create w Output in
    (input, output)

  module Lazy_fiber : sig
    type 'a t

    val create : (unit -> 'a Fiber.t) -> 'a t
    val force : 'a t -> 'a Fiber.t
  end = struct
    type 'a t = {
      value : 'a Fiber.Ivar.t;
      mutable f : (unit -> 'a Fiber.t) option;
    }

    let create f = { f = Some f; value = Fiber.Ivar.create () }

    let force t =
      let open Fiber.O in
      match t.f with
      | None -> Fiber.Ivar.read t.value
      | Some f ->
          Fiber.of_thunk (fun () ->
              t.f <- None;
              let* v = f () in
              let+ () = Fiber.Ivar.fill t.value v in
              v)
  end

  let make_std_fd fd kind =
    Lazy_fiber.create (fun () ->
        let blockity =
          if Sys.win32 then `Blocking
          else (
            Unix.set_nonblock fd;
            `Non_blocking true)
        in
        create (Fd.create fd blockity) kind)

  let stdin = Lazy_fiber.force (make_std_fd Unix.stdin Input)
  let stderr = Lazy_fiber.force (make_std_fd Unix.stderr Output)
  let stdout = Lazy_fiber.force (make_std_fd Unix.stdout Output)
end

module Socket = struct
  let writeable_fd scheduler fd =
    let ivar = Fiber.Ivar.create () in
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
    Fiber.Ivar.read ivar

  let connect (fd : Fd.t) sock =
    let* scheduler = Fiber.Var.get_exn scheduler in
    Fd.set_nonblock fd;
    match Unix.connect fd.fd sock with
    | () -> Fiber.return ()
    | exception Unix.Unix_error (Unix.EISCONN, _, _) -> Fiber.return ()
    | exception Unix.Unix_error (Unix.EINPROGRESS, _, _) -> (
        let+ () = writeable_fd scheduler fd.fd in
        match Unix.getsockopt_error fd.fd with
        | None -> ()
        | Some err -> raise (Unix.Unix_error (err, "connect", "")))

  module Server = struct
    type t = {
      fd : Fd.t;
      pool : Fiber.Pool.t;
      io : Lev.Io.t;
      mutable close : bool;
      mutable await : unit Fiber.Ivar.t;
    }

    let create (fd : Fd.t) sockaddr ~backlog =
      let+ scheduler = Fiber.Var.get_exn scheduler in
      let pool = Fiber.Pool.create () in
      Fd.set_nonblock fd;
      Unix.bind fd.fd sockaddr;
      Unix.listen fd.fd backlog;
      let t = Fdecl.create Dyn.opaque in
      let io =
        Lev.Io.create
          (fun _ _ _ ->
            let t = Fdecl.get t in
            Queue.push scheduler.queue (Fiber.Fill (t.await, ())))
          fd.fd
          (Lev.Io.Event.Set.create ~read:true ())
      in
      Fdecl.set t { pool; await = Fiber.Ivar.create (); close = false; fd; io };
      Fdecl.get t

    let close t =
      Fiber.of_thunk (fun () ->
          if t.close then Fiber.return ()
          else
            let* scheduler = Fiber.Var.get_exn scheduler in
            Fd.close t.fd;
            Lev.Io.stop t.io scheduler.loop;
            Lev.Io.destroy t.io;
            t.close <- true;
            let* () = Fiber.Pool.stop t.pool in
            Fiber.Ivar.fill t.await ())

    module Session = struct
      type t = { fd : Fd.t; sockaddr : Unix.sockaddr }

      let fd t = t.fd
      let sockaddr t = t.sockaddr

      let io t =
        Fd.set_nonblock t.fd;
        Io.create_rw t.fd
    end

    let serve =
      let rec loop t f =
        let* () = Fiber.Ivar.read t.await in
        match t.close with
        | true -> Fiber.return ()
        | false ->
            t.await <- Fiber.Ivar.create ();
            let session =
              let fd, sockaddr = Unix.accept ~cloexec:true t.fd.fd in
              let fd = Fd.create' fd (Non_blocking { set = false }) in
              { Session.fd; sockaddr }
            in
            let* () = Fiber.Pool.task t.pool ~f:(fun () -> f session) in
            loop t f
      in
      fun (t : t) ~f ->
        let* scheduler = Fiber.Var.get_exn scheduler in
        Lev.Io.start t.io scheduler.loop;
        Fiber.fork_and_join_unit
          (fun () -> Fiber.Pool.run t.pool)
          (fun () -> loop t f)
  end
end

let yield () =
  let* scheduler = Fiber.Var.get_exn scheduler in
  let ivar = Fiber.Ivar.create () in
  Queue.push scheduler.queue (Fiber.Fill (ivar, ()));
  Fiber.Ivar.read ivar

exception Finished of process * Unix.process_status

let run lev_loop ~f =
  let tref = Fdecl.create Dyn.opaque in
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
    let process_watcher =
      match Sys.win32 with
      | false -> None
      | true ->
          let watcher =
            {
              active = Table.create (module Pid) 16;
              mutex = Mutex.create ();
              process_thread = lazy ();
            }
          in
          let rec run_thread watcher =
            Mutex.lock watcher.mutex;
            let result = check_running watcher in
            Option.iter result ~f:(fun (job, _) ->
                Table.remove watcher.active job.pid);
            Mutex.unlock watcher.mutex;
            (match result with
            | None -> Unix.sleepf 0.05
            | Some (job, status) ->
                finish_job (Fdecl.get tref) (Fiber.Fill (job.ivar, status)));
            run_thread watcher
          and check_running watcher =
            try
              Table.iter watcher.active ~f:(fun (job : process) ->
                  let pid, status =
                    Unix.waitpid [ WNOHANG ] (Pid.to_int job.pid)
                  in
                  if pid <> 0 then raise_notrace (Finished (job, status)));
              None
            with Finished (job, status) -> Some (job, status)
          in
          watcher.process_thread <-
            lazy (Thread.spawn_thread @@ fun () -> run_thread watcher);
          Some watcher
    in
    let tval =
      {
        loop = lev_loop;
        queue;
        async;
        thread_mutex;
        thread_jobs;
        process_watcher;
      }
    in
    Fdecl.set tref tval;
    Fiber.Var.set t tval f
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
        | `No_more_active_watchers ->
            (* TODO incorrect if there are active threads *)
            iter_or_deadlock q
        | `Otherwise -> iter loop q)
  in
  Fiber.run f ~iter:(fun () -> iter lev_loop queue)
