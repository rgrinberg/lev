# Lev - OCaml bindings to libev

## Abstract

[libev](libev) by Marc Lehmann is a minimal & portable event loop library. Lev
offers low level bindings to this library. The bindings are designed to be
minimal, low overhead, and easily embeddable in larger projects. The API is
callback based so you need to BYOC (bring your own concurrency).

## Example

```ocaml
open Lev

let () =
  let loop = Loop.default () in
  let stdin, stdin_w = Unix.pipe ~cloexec:true () in
  let stdout_r, stdout = Unix.pipe ~cloexec:true () in
  let stderr_r, stderr = Unix.pipe ~cloexec:true () in
  Unix.close stdin_w;
  Unix.close stdout_r;
  Unix.close stderr_r;
  let _pid =
    Unix.create_process "sh" [| "sh"; "-c"; "exit 42" |] stdin stdout stderr
  in
  let child =
    Child.create
      (fun t ~pid status ->
        Child.stop t loop;
        match status with
        | Unix.WEXITED i -> Printf.printf "%d exited with status %d\n" pid i
        | _ -> assert false)
      (Pid pid) Terminate
  in
  Child.start child loop;
  Loop.run_until_done loop
```

## Documentation

Lev's API is a thin wrapper around libev itself. So you should first and
foremost refer to libev's extensive [documentation](libevdoc). Lev itself will
document where it differs from libev's conventions.

## License

`src/` is offered under ISC (see src/LICENSE.md).
`vendor/` is under Marc Lehmann's original terms (see vendor/LICENSE).

[libev]: http://software.schmorp.de/pkg/libev.html
[libevdoc]: http://pod.tst.eu/http://cvs.schmorp.de/libev/ev.pod
