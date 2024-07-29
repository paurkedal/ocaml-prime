(* Copyright (C) 2024  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

module A = Alcotest.V1
open Prime_syntax

module Term = struct

  type t = [`Skip | `Keep] * int list option

  let comp (f : int -> 'a -> 'a) (terms : t list) : 'a -> 'a =
    let^@ insn, args_opt = terms in
    match insn with
    | `Skip ->
        Fun.id
    | `Keep ->
        let^? args = args_opt in
        let^@ arg = args in
        f arg

end

let test_comp () =
  A.(check int) "sum" 7
    (0 |> Term.comp (+) [
      `Keep, None;
      `Keep, Some [1; 2];
      `Skip, Some [3];
      `Keep, Some [4];
    ])

let test_cases = [
  A.test_case "comp" `Quick test_comp;
]
