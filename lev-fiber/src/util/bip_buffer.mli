(** Simon Cooke's Bip_buffer *)

module Blit : sig
  type ('src, 'dst) t =
    src:'src -> src_pos:int -> dst:'dst -> dst_pos:int -> len:int -> unit
end

module Slice : sig
  type t = { pos : int; len : int }
end

type 'a t

val is_empty : 'a t -> bool

val length : 'a t -> int

val buffer : 'a t -> 'a

val create : 'a -> len:int -> 'a t

val junk : 'a t -> len:int -> unit

val peek : 'a t -> Slice.t option

val reserve : 'a t -> len:int -> int option

val commit : 'a t -> len:int -> unit

val compress_gain : 'a t -> int

val compress : 'a t -> ('a, 'a) Blit.t -> unit
(** [compress t blit] will try to compress the buffer with 2 blit operations.
    Use [compres_gain t] to asses how useful this will be. *)

val resize : 'a t -> ('a, 'b) Blit.t -> 'b -> len:int -> 'b t
(** [resize t blit buf ~len] will create a new buffer with the same data. The
    old buffer is then emptied and can be reused *)
