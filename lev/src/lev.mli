(** Thin wrapper around Marc Lehmann's libev library.

libev is small and performant, but this comes at a cost of a few rough edges
and a higher learning curve. Lev tries to make it more usable, but it does not
try to invent a new API. You should be somewhat familiar with async IO and
libev's quirks to use this library effectively.

Some things to keep in mind:

  - It is your responsibility to make sure that fd's are non blocking

  - Watchers are mutable and the event loop will mutate them. In libev, it is
  possible for the user to mutate the watchers as well, but [Lev] does not
  provide this because it is unsafe and mostly unnecessary. Just crate new
  watchers and throw out the old ones.

  - All actual read, write, accept, etc. calls are outside the scope of this
    library.

  - You must call [$Watcher.destroy] to free the resources taken of every
  watcher. A similar rule applies for [Loop.destroy], but it usually does not
  matter.

  - You must not reuse the same watcher between different event loops.

  - Watchers are not threadsafe with the exception of [Async]. [Async] is the
  specifically designed to wake up the event loop from different threads.

 *)

val ev_version : unit -> int * int

module Backend : sig
  type t =
    | Select
    | Poll
    | Epoll
    | Kqueue
    | Devpoll
    | Port
    | Linuxaio
    | Iouring

  module Set : sig
    type t

    val all : t

    type backend

    val mem : t -> backend -> bool
  end
  with type backend := t

  val supported : unit -> Set.t
  val embeddable : unit -> Set.t
  val recommended : unit -> Set.t
end

module Timestamp : sig
  type t

  val sleep : t -> unit
  val of_float : float -> t
  val to_float : t -> float
end

module Loop : sig
  module Flag : sig
    type t =
      | Backend of Backend.t
      | Auto
      | Noenv
      | Forkcheck
      | Noinotify
      | Signalfd
      | Nosigmask
      | Notimerfd

    val to_int : t -> int

    module Set : sig
      type elt := t
      type t

      val empty : t
      val singleton : elt -> t
      val of_backend_set : Backend.Set.t -> t
      val inter : t -> t -> t
      val union : t -> t -> t
      val negate : t -> t
      val mem : t -> elt -> bool
    end
  end

  type t

  val now : t -> Timestamp.t

  val default : ?flags:Flag.Set.t -> unit -> t
  (** The default event loop is the only one that can handle child watchers. *)

  val is_default : t -> bool
  val create : ?flags:Flag.Set.t -> unit -> t
  val destroy : t -> unit
  val now_update : t -> unit

  type run = Once | Nowait

  val run : t -> run -> [ `No_more_active_watchers | `Otherwise ]
  val run_until_done : t -> unit
  val depth : t -> int

  type break = One | All | Cancel

  val break : t -> break -> unit
  val backend : t -> Backend.t
  val suspend : t -> unit
  val resume : t -> unit
  val ref : t -> unit
  val unref : t -> unit
  val feed_signal : signal:int -> unit
  val feed_signal_event : t -> signal:int -> unit
end

module type Watcher = sig
  type t
  (** The basic unit of interactions with an event loop. Created with a
      callback that the event loop will trigger once the watcher becomes pending. *)

  val start : t -> Loop.t -> unit
  (** [start t loop] starts the watcher and associates it to [loop]. You must
      not use this watcher in another event loop. *)

  val is_active : t -> bool
  (** [is_active t] returns [true] if [start t loop] has been called but [stop
      t] has not. *)

  val is_pending : t -> bool
  (** [is_pending t] returns [true] if the callback associated with [t] needs
      to be invoked. *)

  val stop : t -> Loop.t -> unit
  (** [stop t loop] the event loop will no longer trigger this watcher. [t] may
      be started again with [t] *)

  val destroy : t -> unit
  (** [destroy t] frees the memory allocated for [t]. After this call, you may
      not use [t] in any way. You may only call [destroy t] if [is_active t]
    and [is_pending t] are both [false]*)
end

module Periodic : sig
  include Watcher

  type kind =
    | Regular of { offset : Timestamp.t; interval : Timestamp.t option }
    | Custom of (t -> now:Timestamp.t -> Timestamp.t)

  val create : (t -> unit) -> kind -> t
end

module Io : sig
  module Event : sig
    type t = Read | Write

    module Set : sig
      type t
      type event

      val equal : t -> t -> bool
      val mem : t -> event -> bool
      val create : ?read:bool -> ?write:bool -> unit -> t
      val add : t -> event -> t
      val inter : t -> t -> t
    end
    with type event := t
  end

  include Watcher

  val fd : t -> Unix.file_descr
  val modify : t -> Event.Set.t -> unit

  val create :
    (t -> Unix.file_descr -> Event.Set.t -> unit) ->
    Unix.file_descr ->
    Event.Set.t ->
    t
end

module Timer : sig
  include Watcher

  val create : ?repeat:float -> after:float -> (t -> unit) -> t
  val remaining : t -> Loop.t -> Timestamp.t
  val again : t -> Loop.t -> unit
end

module Stat : sig
  include Watcher

  val stat : t -> Unix.stats
  (** [stat t] is only permitted to be called inside the callback to [create] *)

  val create :
    ( ?interval:Timestamp.t -> path:string -> (t -> unit) -> t,
      [ `Unimplemented ] )
    result
end

module Child : sig
  include Watcher

  type pid = Any | Pid of int
  type trace = Terminate | Terminate_stop_or_continue

  val create :
    ( (t -> pid:int -> Unix.process_status -> unit) -> pid -> trace -> t,
      [ `Unimplemented ] )
    result
end

module Signal : sig
  include Watcher

  val create : (t -> unit) -> signal:int -> t
end

module Cleanup : sig
  include Watcher

  val create : (t -> unit) -> t
end

module Async : sig
  include Watcher

  val create : (t -> unit) -> t
  val send : t -> Loop.t -> unit
  val pending : t -> bool
end

module Check : sig
  include Watcher

  val create : (t -> unit) -> t
end

module Prepare : sig
  include Watcher

  val create : (t -> unit) -> t
end

module Idle : sig
  include Watcher

  val create : (t -> unit) -> t
end

module Embed : sig
  include Watcher

  type sweep = Automatic | Manual of (t -> unit)

  val create : sweep -> Loop.t -> t
  val sweep : t -> Loop.t -> unit
end
