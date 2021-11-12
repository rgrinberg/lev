external ev_version : unit -> int * int = "lev_version"

module Loop = struct
  type t

  external default : unit -> t = "lev_ev_default"

  external create : unit -> t = "lev_ev_create"

  let destroy _ = ()

  let now_update _ = ()

  external run : t -> bool = "lev_ev_run"

  let run t _ = if run t then `Otherwise else `No_more_active_watchers

  let depth _ = 0

  let break _ _ = ()
end

module Status = struct
  type t = Pending | Active
end

module Timestamp = struct
  type t
end

module type Watcher = sig
  type t

  val status : t -> Status.t

  val start : t -> Loop.t -> unit

  val stop : t -> Loop.t -> unit
end

module Io = struct
  module Event = struct
    type t = Read | Write

    module Set = struct
      type t = int

      let mem _ _ = assert false

      external create : bool -> bool -> t = "lev_io_event_create"

      let create ?(read = false) ?(write = false) () = create read write
    end
  end

  type t

  external fd : t -> Unix.file_descr = "lev_io_fd"

  external create :
    (t -> Loop.t -> Event.Set.t -> unit) -> Unix.file_descr -> int -> t
    = "lev_io_create"

  external start : t -> Loop.t -> unit = "lev_io_start"

  external stop : t -> Loop.t -> unit = "lev_io_stop"

  let status _ = assert false
end
