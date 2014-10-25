(* Copyright (C) 2013--2014  Petter Urkedal <paurkedal@gmail.com>
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
module Int_set = Set.Make (Int_order)
module Int_eset = Prime_enumset.Make (Int_order)

let run () =
  for round = 0 to 999 do
    let rec populate imax n s es =
      if n < 0 then (s, es) else
      let i = Random.int imax in
      let j = Random.int imax in
      let es' = Int_eset.remove i es in
      let es'' = Int_eset.add j es' in
      assert (not (Int_eset.contains i es'));
      assert (Int_eset.contains j es'');
      populate imax (n - 1) (Int_set.add j (Int_set.remove i s)) es'' in
    let n = Random.int (1 lsl Random.int 10) + 1 in
    let s, es = populate n n Int_set.empty Int_eset.empty in
    assert_equal_int ~msg:"cardinality using fold" (Int_set.cardinal s)
		     (Int_eset.fold (fun _ -> (+) 1) es 0);
    assert_equal_int ~msg:"card" (Int_set.cardinal s) (Int_eset.card es);
    assert_equal ~msg:"elements" (Int_set.elements s)
		 (List.rev (Int_eset.fold Prime_list.push es []));
    assert_equal_int ~msg:"min element" (Int_eset.min_elt es)
		     (Int_eset.get 0 es);
    assert_equal_int ~msg:"max element" (Int_eset.max_elt es)
		     (Int_eset.get (Int_eset.card es - 1) es);
    for i = 0 to Int_eset.card es - 1 do
      let e = Int_eset.get i es in
      let pres, pos = Int_eset.locate e es in
      assert pres;
      assert_equal_int ~msg:"locate (get i es)" i pos
    done
  done
