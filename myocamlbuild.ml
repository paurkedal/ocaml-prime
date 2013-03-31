(* OASIS_START *)
(* OASIS_STOP *)

let () = dispatch begin function

  | After_rules as e ->
    flag_and_dep ["doc"; "ocaml"; "extension:html"] &
      S[A"-charset"; A"utf8"; A"-colorize-code";
	A"-i"; A"doc"; A"-g"; A"primedoc.cma";
	A"-t"; A"The OCaml Prime Library";
	A"-passrest"; A"-copy-css-style"; P"doc/odoc-bright.css"];
    dep ["doc"; "ocaml"] ["doc/primedoc.cma"];
    dispatch_default e

  | e ->
    dispatch_default e

end
