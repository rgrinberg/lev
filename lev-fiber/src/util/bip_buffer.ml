open Stdune

module Slice = struct
  type t = { pos : int; len : int }
end

type 'a t = {
  buf : 'a;
  buf_len : int;
  mutable a_start : int;
  mutable a_end : int;
  mutable b_end : int;
  mutable b_inuse : bool;
  mutable reserving : bool;
}

let buffer t = t.buf

let _invariant t =
  if t.b_inuse then assert (t.b_end - t.a_start > t.buf_len - t.a_end)

let create buf ~len =
  {
    buf;
    buf_len = len;
    a_start = 0;
    a_end = 0;
    b_end = 0;
    b_inuse = false;
    reserving = false;
  }

let peek t =
  if t.a_start < t.a_end then Some { Slice.pos = t.a_start; len = t.a_end }
  else None

let is_empty t = t.a_start = t.a_end

let length t = t.a_end - t.a_start + t.b_end

let junk t size =
  assert (length t >= size);
  let read_len = min size (t.a_end - t.a_start) in
  t.a_start <- t.a_start + read_len;
  let size = read_len - size in
  if size >= 0 then (
    t.b_inuse <- false;
    t.a_start <- 0;
    t.a_end <- t.b_end;
    t.b_end <- 0)

let unused t = if t.b_inuse then t.a_start - t.b_end else t.buf_len - t.a_end

let write_start t = if t.b_inuse then t.b_end else t.a_end

let set_current_buffer t =
  let space_left_for_a = t.buf_len - t.a_end in
  let space_left_for_b = t.a_start - t.b_end in
  if space_left_for_a < space_left_for_b then t.b_inuse <- true

let reserve t size =
  if t.reserving then Code_error.raise "previous reserve not committed" [];
  if unused t < size then None
  else (
    t.reserving <- true;
    Some (write_start t))

let commit t ~len =
  assert t.reserving;
  if t.b_inuse then t.b_end <- t.b_end + len else t.a_end <- t.a_end + len;
  t.reserving <- false;
  set_current_buffer t
