open Stdune
module Bytes = BytesLabels
module B = Util.Bip_buffer

let%expect_test "create" =
  let len = 128 in
  let b = B.create (Bytes.create len) ~len in
  let unused = B.unused b in
  printfn "%d = %d" unused len;
  assert (unused = len);
  [%expect {| 128 = 128 |}]

let%expect_test "is_empty" =
  let len = 100 in
  assert (B.is_empty (B.create (Bytes.create len) ~len));
  [%expect {||}]

let%expect_test "read empty" =
  let b = B.create (Bytes.create 0) ~len:1 in
  assert (B.peek b = None);
  [%expect {||}]

let%expect_test "bip buffers" =
  let buf_size = 100 in
  let b = B.create (Bytes.create buf_size) ~len:buf_size in
  assert (B.is_empty b);
  printfn "Unused space: %d" (B.unused b);
  [%expect {| Unused space: 100 |}];
  let mystr = "Test Foo Bar" in
  let mystr_len = String.length mystr in
  let dst_pos = Option.value_exn (B.reserve b mystr_len) in
  let buf = B.buffer b in
  Bytes.blit_string ~dst:buf ~dst_pos ~src:mystr ~src_pos:0 ~len:mystr_len;
  B.commit b ~len:mystr_len;
  assert (B.length b = mystr_len);
  let unused = B.unused b in
  let expected = buf_size - mystr_len in
  printfn "unused %d. Expected %d" unused expected;
  assert (unused = expected);
  [%expect {| unused 88. Expected 88 |}];
  (* Now we try to read 4 characters *)
  let () =
    let slice = Option.value_exn (B.peek b) in
    let read_len = 4 in
    assert (slice.len >= read_len);
    let read = Bytes.create read_len in
    Bytes.blit ~src:(B.buffer b) ~src_pos:slice.pos ~dst:read ~dst_pos:0
      ~len:read_len;
    printfn "Read: %s" (Bytes.to_string read);
    B.junk b read_len
  in
  [%expect {| Read: Test |}]
