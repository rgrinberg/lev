(** Simon Cooke's Bip_buffer *)

module Blit : sig
  type ('src, 'dst) t =
    src:'src -> src_pos:int -> dst:'dst -> dst_pos:int -> len:int -> unit
end

module Slice : sig
  type t = { pos : int; len : int }
end

type 'a t

val available : _ t -> int
val is_empty : _ t -> bool
val length : _ t -> int
val buffer : 'a t -> 'a
val create : 'a -> len:int -> 'a t
val junk : _ t -> len:int -> unit
val peek : _ t -> Slice.t option
val reserve : _ t -> len:int -> int option
val commit : _ t -> len:int -> unit

val unused_space : _ t -> int
(** Total amount of free space available in the buffer. Not all of it may be
    usable. To reclaim it, call [compress] *)

val compress : 'a t -> ('a, 'a) Blit.t -> unit
(** [compress t blit] will try to compress the buffer with 2 blit operations.
    Use [unused_space t] to asses how useful this will be. *)

val resize : 'a t -> ('a, 'a) Blit.t -> 'a -> len:int -> unit
(** [resize t blit buf ~len] will create a new buffer with the same data. The
    old buffer is then emptied and can be reused *)

val pp :
  (Format.formatter -> 'a * Slice.t -> unit) -> Format.formatter -> 'a t -> unit
