(* Copyright (C) 2015--2024  Petter A. Urkedal <paurkedal@gmail.com>
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
module Int_order = struct type t = int let compare = compare end

module List_monoid = struct
  type 'a t = 'a list
  let empty = []
  let cat = (@)
end

module Am = Prime_accretion_map.Make1 (Int_order) (List_monoid)
module Rm = Map.Make (Int_order)

let test_special_cases () =
  A.(check bool) "is_empty empty" true (Am.is_empty Am.empty);
  A.(check bool) "is_empty singleton" false (Am.is_empty (Am.singleton 0 []));
  A.(check int) "candinal empty" 0 (Am.cardinal Am.empty);
  A.(check int) "candinal singleton" 1 (Am.cardinal (Am.singleton 0 []));
  A.(check (list int)) "result empty" [] (Am.result Am.empty);
  A.(check (list int)) "result singleton" [11] (Am.result (Am.singleton 0 [11]))

let test_random' n =
  let aux i (rm, am) =
    let k = Random.int n in
    let x = [i] in
    Rm.add k x rm, Am.add k x am
  in
  let rm, am = Prime_int.fold_to aux n (Rm.empty, Am.empty) in
  A.(check int) "candinal"
    (Rm.cardinal rm) (Am.cardinal am);
  A.(check (list (pair int (list int)))) "bindings"
    (Rm.bindings rm) (Am.bindings am);
  A.(check (list int)) "result"
    (List.rev (Rm.fold (fun _ x acc -> x @ acc) rm []))
    (Am.result am)

let test_random () =
  for i = 1 to 200 do
    test_random' i;
    test_random' i;
    test_random' i
  done

let test_cases = [
  A.test_case "special cases" `Quick test_special_cases;
  A.test_case "random" `Quick test_random;
]
