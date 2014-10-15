(* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the OCaml static compilation exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 *)

open OUnit
open Utils
open Unprime_option

module Int_order = struct type t = int let compare = compare end
module Int_map = Map.Make (Int_order)
module Int_emap = Prime_enummap.Make (Int_order)

let run () =
  for round = 0 to 999 do
    let rec populate imax n m em =
      if n < 0 then (m, em) else
      let i = Random.int imax in
      let j = Random.int imax in
      let em' = Int_emap.remove i em in
      let em'' = Int_emap.add j (j + 1) em' in
      assert (not (Int_emap.contains i em'));
      assert (Int_emap.contains j em'');
      assert_equal (Int_emap.find j em'') (j + 1);
      populate imax (n - 1) (Int_map.add j (j + 1) (Int_map.remove i m)) em'' in
    let n = Random.int (1 lsl Random.int 10) + 1 in
    let m, em = populate n n Int_map.empty Int_emap.empty in
    assert_equal_int ~msg:"cardinality using fold" (Int_map.cardinal m)
		     (Int_emap.fold (fun _ _ -> (+) 1) em 0);
    assert_equal_int ~msg:"card" (Int_map.cardinal m) (Int_emap.card em);
    assert_equal ~msg:"min binding" (Int_emap.min_binding em)
		 (Int_emap.get_binding 0 em);
    assert_equal ~msg:"max binding" (Int_emap.max_binding em)
		 (Int_emap.get_binding (Int_emap.card em - 1) em);
    for i = 0 to Int_emap.card em - 1 do
      let k, _ = Int_emap.get_binding i em in
      assert_equal_int ~msg:"locate (get i em)" i (Int_emap.locate_e k em)
    done
  done
