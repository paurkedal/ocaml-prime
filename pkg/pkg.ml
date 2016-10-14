#! /usr/bin/env ocaml
#use "topfind"
#require "adpkg"
#require "topkg"
open Adpkg
open Topkg

let licenses = List.map Pkg.std_file ["COPYING.LESSER"; "COPYING"]

(* The ounit package is omitted from opam since the testsuite is only built in
 * development context. *)
let opams = [Pkg.opam_file "opam" ~lint_deps_excluding:(Some ["oUnit"])]

let () = Pkg.describe ~licenses ~opams "prime" @@ fun c ->
  Modules.of_file "lib/prime.oclib" >>= fun modules ->
  Modules.mllib modules "lib/prime.mllib"
    ~filter:Filter.(not (tagged "unstable")) >>= fun prime_mllib ->
  Modules.mllib modules "lib/prime-unstable.mllib"
    ~filter:Filter.(tagged "unstable") >>= fun prime_unstable_mllib ->
  Modules.save modules "doc/api.odocl" >>= fun () ->
  Ok [
    prime_mllib;
    prime_unstable_mllib;
    Pkg.test "tests/testsuite";
  ]
