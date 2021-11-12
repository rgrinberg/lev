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
    (t -> Loop.t -> Event.Set.t -> unit) -> Unix.file_descr -> int -> t
    = "lev_io_create"

  external start : t -> Loop.t -> unit = "lev_io_start"

  external stop : t -> Loop.t -> unit = "lev_io_stop"

  let status _ = assert false
end
