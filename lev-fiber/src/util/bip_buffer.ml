open Stdune

module Blit = struct
  type ('src, 'dst) t =
    src:'src -> src_pos:int -> dst:'dst -> dst_pos:int -> len:int -> unit
end

module Slice = struct
  type t = { pos : int; len : int }
end

type 'a t = {
  mutable buf : 'a;
  mutable buf_len : int;
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
  if t.a_start < t.a_end then
    Some { Slice.pos = t.a_start; len = t.a_end - t.a_start }
  else None

let is_empty t = t.a_start = t.a_end
let length t = t.a_end - t.a_start + t.b_end

let junk t ~len:size =
  assert (length t >= size);
  let read_len = min size (t.a_end - t.a_start) in
  t.a_start <- t.a_start + read_len;
  if t.a_start = t.a_end then (
    t.a_start <- size - read_len;
    t.a_end <- t.b_end;
    t.b_end <- 0;
    t.b_inuse <- false)

let space_left_for_a t = t.buf_len - t.a_end
let space_left_for_b t = t.a_start - t.b_end

(* the maximum buffer that doesn't fragment the buffer *)
let best_available t =
  if t.b_inuse then space_left_for_b t else space_left_for_a t

(* the maximum space for a single contiguous write *)
let max_available t =
  if t.b_inuse then space_left_for_b t
  else max (space_left_for_a t) (space_left_for_b t)

let reserve t ~len:size =
  if t.reserving then Code_error.raise "previous reserve not committed" [];
  let space_left_for_a = space_left_for_a t in
  let space_left_for_b = space_left_for_b t in
  if t.b_inuse then
    if space_left_for_b >= size then (
      t.reserving <- true;
      Some t.b_end)
    else None
  else if space_left_for_a >= size then (
    t.reserving <- true;
    Some t.a_end)
  else if space_left_for_b >= size then (
    t.reserving <- true;
    t.b_inuse <- true;
    Some t.b_end)
  else None

let commit t ~len =
  assert t.reserving;
  if t.b_inuse then (
    assert (t.b_end + len <= t.a_start);
    t.b_end <- t.b_end + len)
  else (
    assert (t.a_end + len <= t.buf_len);
    t.a_end <- t.a_end + len);
  t.reserving <- false

let unused_space t = space_left_for_a t + space_left_for_b t

let compress t (blit : (_, _) Blit.t) =
  assert (not t.reserving);
  if t.a_start = 0 then ()
  else
    let src = t.buf in
    let dst = t.buf in
    if t.b_end = 0 then (
      let len_a = t.a_end - t.a_start in
      blit ~src ~dst ~src_pos:t.a_start ~dst_pos:0 ~len:len_a;
      t.a_start <- 0;
      t.b_end <- len_a;
      t.b_end <- 0;
      t.b_inuse <- false)
    else if space_left_for_b t >= t.a_end - t.a_start then (
      let len_a = t.a_end - t.a_start in
      blit ~src ~dst ~src_pos:0 ~dst_pos:len_a ~len:t.b_end;
      blit ~src ~dst ~src_pos:t.a_start ~dst_pos:0 ~len:len_a;
      t.a_start <- 0;
      t.a_end <- len_a + t.b_end;
      t.b_end <- 0;
      t.b_inuse <- false)

let resize t (blit : (_, _) Blit.t) dst ~len =
  assert (not t.reserving);
  let len_t = length t in
  assert (len >= len_t);
  let src = t.buf in
  let len_a = t.a_end - t.a_start in
  blit ~src ~dst ~src_pos:t.a_start ~dst_pos:0 ~len:len_a;
  blit ~src ~dst ~src_pos:0 ~dst_pos:len_a ~len:t.b_end;
  t.buf <- dst;
  t.buf_len <- len;
  t.a_start <- 0;
  t.b_end <- 0;
  t.a_end <- len_t;
  t.b_inuse <- false;
  t.a_start <- 0

let pp pp_slice fmt t =
  let slice = { Slice.pos = t.a_start; len = t.a_end - t.a_start } in
  pp_slice fmt (t.buf, slice);
  if t.b_end > 0 then
    let slice = { Slice.pos = 0; len = t.b_end } in
    pp_slice fmt (t.buf, slice)
