open Stdune

module Buffer : sig
  type t

  val create : size:int -> t
end

module Timer : sig
  val sleepf : float -> unit Fiber.t
  (** [sleep f] wait for [f] seconds  *)

  module Wheel : sig
    type t
    (** wheel to handle many constant timeouts efficiently *)

    val create : delay:float -> t Fiber.t
    (** [create ~delay] will create a wheel that times out every task in [delay] *)

    type task
    (** a task scheduled by the timer wheel *)

    val reset : task -> unit

    val task : t -> task Fiber.t
    (** create a new task *)

    val await : task -> [ `Ok | `Cancelled ] Fiber.t
    (** wait for delay seconds *)

    val cancel : task -> unit Fiber.t
    (** cancel waiting *)

    val run : t -> unit Fiber.t
    (** run the wheel forever *)

    val stop : t -> unit Fiber.t
    (** [stop t] stop running the wheel and cancel everything. it's an error to call [task t] after this *)
  end
end

val wait : pid:int -> Unix.process_status Fiber.t

module Thread : sig
  type t

  val create : unit -> t Fiber.t

  type 'a task

  val task : t -> f:(unit -> 'a) -> 'a task Fiber.t

  val cancel : 'a task -> unit Fiber.t

  val await :
    'a task ->
    ('a, [ `Exn of Exn_with_backtrace.t | `Cancelled ]) result Fiber.t

  val close : t -> unit
end

module Io : sig
  type input

  type output

  type 'a mode = Input : input mode | Output : output mode

  type 'a t

  val create :
    Unix.file_descr ->
    Buffer.t ->
    [ `Blocking | `Non_blocking ] ->
    'a mode ->
    'a t Fiber.t

  val create_rw :
    Unix.file_descr ->
    input:Buffer.t ->
    output:Buffer.t ->
    [ `Blocking | `Non_blocking ] ->
    (input t * output) Fiber.t

  val fd : 'a t -> Unix.file_descr

  val flush : output t -> unit Fiber.t

  module Slice : sig
    type t

    val length : t -> int

    val get : t -> int -> char

    val sub : t -> pos:int -> len:int -> string

    val consume : t -> int -> unit
  end

  type reader

  val read : ?max:int -> reader -> Slice.t option Fiber.t

  val with_read : input t -> f:(reader -> 'a Fiber.t) -> 'a Fiber.t

  val closed : 'a t -> unit Fiber.t

  val close : 'a t -> unit

  val pipe : unit -> input t * output t
end

module Socket : sig
  module Server : sig
    type t

    val create : Unix.sockaddr -> backlog:int -> t Fiber.t

    val stop : t -> unit

    val serve :
      t -> f:(Unix.sockaddr -> Unix.file_descr -> unit Fiber.t) -> unit Fiber.t

    val listening_address : t -> Unix.sockaddr
  end

  module Client : sig
    type t

    val create : Unix.sockaddr -> t Fiber.t

    val connect : t -> Unix.file_descr Fiber.t

    val stop : t -> unit
  end
end

val run : Lev.Loop.t -> f:(unit -> 'a Fiber.t) -> 'a
