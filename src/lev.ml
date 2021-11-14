module List = ListLabels

external ev_version : unit -> int * int = "lev_version"

external feed_signal : int -> unit = "lev_feed_signal"

module Backend = struct
  type t =
    | Select
    | Poll
    | Epoll
    | Kqueue
    | Devpoll
    | Port
    | Linuxaio
    | Iouring

  let all = [ Select; Poll; Epoll; Kqueue; Devpoll; Port; Linuxaio; Iouring ]

  external select : unit -> int = "lev_backend_select"

  let select = select ()

  external poll : unit -> int = "lev_backend_poll"

  let poll = poll ()

  external epoll : unit -> int = "lev_backend_epoll"

  let epoll = epoll ()

  external kqueue : unit -> int = "lev_backend_kqueue"

  let kqueue = kqueue ()

  external devpoll : unit -> int = "lev_backend_devpoll"

  let devpoll = devpoll ()

  external port : unit -> int = "lev_backend_port"

  let port = port ()

  external linuxaio : unit -> int = "lev_backend_linuxaio"

  let linuxaio = linuxaio ()

  external iouring : unit -> int = "lev_backend_iouring"

  let iouring = iouring ()

  let to_int = function
    | Select -> select
    | Poll -> poll
    | Epoll -> epoll
    | Kqueue -> kqueue
    | Devpoll -> devpoll
    | Port -> port
    | Linuxaio -> linuxaio
    | Iouring -> iouring

  module Set = struct
    type t = int

    let mem t c = t land to_int c <> 0
  end

  external supported : unit -> Set.t = "lev_backend_supported"

  external embeddable : unit -> Set.t = "lev_backend_embeddable"

  external recommended : unit -> Set.t = "lev_backend_recommended"
end

module Timestamp = struct
  type t = float

  external sleep : t -> unit = "lev_sleep"

  let to_float x = x

  let of_float x = x
end

module Loop = struct
  type t

  external default : unit -> t = "lev_ev_default"

  external create : unit -> t = "lev_ev_create"

  external now : t -> Timestamp.t = "lev_ev_now"

  let destroy _ = ()

  let now_update _ = ()

  external run : t -> bool = "lev_ev_run"

  let run t = if run t then `Otherwise else `No_more_active_watchers

  let rec run_until_done t =
    match run t with
    | `Otherwise -> run_until_done t
    | `No_more_active_watchers -> ()

  let depth _ = 0

  let break _ _ = ()

  external backend : t -> Backend.Set.t = "lev_loop_backend"

  let backend t =
    let b = backend t in
    List.find Backend.all ~f:(fun backend -> Backend.Set.mem b backend)

  let suspend _ = ()

  let resume _ = ()
end

module type Watcher = sig
  type t

  val start : t -> Loop.t -> unit

  val stop : t -> Loop.t -> unit
end

module Io = struct
  module Event = struct
    type t = Read | Write

    external read : unit -> int = "lev_io_read_code"

    let read = read ()

    external write : unit -> int = "lev_io_write_code"

    let write = write ()

    let to_int = function Read -> read | Write -> write

    module Set = struct
      type t = int

      let mem t c = t land to_int c <> 0

      let create ?(read = false) ?(write = false) () =
        (if read then to_int Read else 0) lor if write then to_int Write else 0
    end
  end

  type t

  external fd : t -> Unix.file_descr = "lev_io_fd"

  external create :
    (t -> Unix.file_descr -> Event.Set.t -> unit) -> Unix.file_descr -> int -> t
    = "lev_io_create"

  external start : t -> Loop.t -> unit = "lev_io_start"

  external stop : t -> Loop.t -> unit = "lev_io_stop"
end

module Periodic = struct
  type t

  type kind =
    | Regular of { offset : Timestamp.t; interval : Timestamp.t option }
    | Custom of (t -> now:Timestamp.t -> Timestamp.t)

  external create_regular : (t -> Loop.t -> unit) -> float -> float -> t
    = "lev_periodic_create_regular"

  external create_custom :
    (t -> Loop.t -> unit) -> (t -> now:Timestamp.t -> Timestamp.t) -> t
    = "lev_periodic_create_custom"

  let create f kind =
    match kind with
    | Custom rb -> create_custom f rb
    | Regular { offset; interval } ->
        let interval = match interval with None -> 0. | Some f -> f in
        create_regular f offset interval

  external stop : t -> Loop.t -> unit = "lev_periodic_stop"

  external start : t -> Loop.t -> unit = "lev_periodic_start"
end

module Timer = struct
  type t

  external create : (t -> Loop.t -> unit) -> float -> float -> t
    = "lev_timer_create"

  let create ?(repeat = 0.) ~after f = create f repeat after

  external remaining : t -> Loop.t -> Timestamp.t = "lev_timer_remaining"

  external stop : t -> Loop.t -> unit = "lev_timer_stop"

  external start : t -> Loop.t -> unit = "lev_timer_start"

  external again : t -> Loop.t -> unit = "lev_timer_again"
end

module Signal = struct
  type t

  let start _ _ = assert false

  let stop _ _ = assert false

  let create _ = assert false
end

module Child = struct
  type t

  let start _ _ = assert false

  let stop _ _ = assert false

  let create _ = assert false
end

module Cleanup = struct
  type t

  let start _ _ = assert false

  let stop _ _ = assert false

  let create _ = assert false
end

module Stat = struct
  type t

  let start _ _ = assert false

  let stop _ _ = assert false

  let create ?interval _ ~path:_ =
    ignore interval;
    assert false
end
