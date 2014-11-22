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
open Unprime
open Unprime_array
open Unprime_option

module Int_order = struct type t = int let compare = compare end
module Int_map = Map.Make (Int_order)
module Int_emap = Prime_enummap.Make (Int_order)

let random_emap n_max =
  let n = Random.int n_max in
  Prime_int.fold_to (fun v -> Int_emap.add (Random.int n_max) v) n
		    Int_emap.empty

let test_equal () =
  let kvs0 = Prime_array.sample (fun _ -> Random.int 40) 40 in
  let kvs1 = Array.copy kvs0 in
  Array.sort compare kvs1;
  let m0 = Array.fold (fun k -> Int_emap.add k k) kvs0 Int_emap.empty in
  let m1 = Array.fold (fun k -> Int_emap.add k k) kvs1 Int_emap.empty in
  assert (Int_emap.equal (=) m0 m1);
  assert (Int_emap.compare compare m0 m1 = 0);
  let m2 =
    let i = Random.int (Array.length kvs0) in
    if Random.bool () then Int_emap.remove kvs0.(i) m0
		      else Int_emap.add kvs0.(i) (-1) m0 in
  assert (not (Int_emap.equal (=) m0 m2));
  assert (not (Int_emap.equal (=) m1 m2));
  let c02 = Int_emap.compare compare m0 m2 in
  let c12 = Int_emap.compare compare m1 m2 in
  let c20 = Int_emap.compare compare m2 m0 in
  let c21 = Int_emap.compare compare m2 m1 in
  assert (c02 <> 0);
  assert (c12 <> 0);
  assert (c20 = - c02);
  assert (c21 = - c12)

let test_cut () =
  let n = 40 in
  let es = Prime_array.sample (fun _ -> Random.int n) n in
  let m = Array.fold (fun e -> Int_emap.add e e) es Int_emap.empty in
  Array.sort compare es;
  let i_cut = Random.int (Array.length es) in
  let e_cut = es.(i_cut) in
  let mL, mR =
    Array.fold
      (fun e (mL, mR) ->
	if e < e_cut then (Int_emap.add e e mL, mR) else
	if e > e_cut then (mL, Int_emap.add e e mR) else
	(mL, mR))
      es (Int_emap.empty, Int_emap.empty) in
  let e_opt, mL', mR' = Int_emap.cut es.(i_cut) m in
  assert (e_opt = Some e_cut);
  assert (Int_emap.equal (=) mL mL');
  assert (Int_emap.equal (=) mR mR')

let test_alg () =
  let n_max = 1 lsl Random.int 8 in
  let mA = random_emap n_max in
  let mB = random_emap n_max in
  let mAnB = Int_emap.finter (fun _ x y -> Some (max x y)) mA mB in
  let mAnB' = Int_emap.merge (fun _ -> Option.inter max) mA mB in
  let mAnB'' = Int_emap.filter (fun k _ -> Int_emap.contains k mB) mA in
  assert (Int_emap.equal (=) mAnB mAnB');
  assert (Int_emap.equal (>=) mAnB mAnB'');
  let mAuB = Int_emap.funion (fun _ x y -> Some (max x y)) mA mB in
  let mAuB' = Int_emap.merge (fun _ -> Option.union max) mA mB in
  let mAuB'' = Int_emap.fold Int_emap.add mA mB in
  assert (Int_emap.equal (=) mAuB mAuB');
  assert (Int_emap.equal (>=) mAuB mAuB'');
  let mAcB = Int_emap.fcompl (fun _ _ _ -> None) mA mB in
  let mAcB' = Int_emap.merge (fun _ -> Option.fcompl (fun _ _ -> None)) mA mB in
  let mAcB'' = Int_emap.fold (fun k _ -> Int_emap.remove k) mA mB in
  assert (Int_emap.equal (=) mAcB mAcB');
  assert (Int_emap.equal (=) mAcB mAcB'');
  let mAdB = Int_emap.fcompl (fun _ x y -> Some (max x y)) mA mB in
  let mAdB' = Int_emap.merge (fun _ -> Option.compl max) mA mB in
  assert (Int_emap.equal (=) mAdB mAdB');
  assert (Int_emap.equal (>=) mAdB mB);
  let mAsB = Int_emap.funion (fun _ _ _ -> None) mA mB in
  assert (Int_emap.cardinal mAuB =
	  Int_emap.cardinal mAsB + Int_emap.cardinal mAnB);
  assert (Int_emap.cardinal mAcB =
	  Int_emap.cardinal mB - Int_emap.cardinal mAnB)

let run () =
  assert (Int_emap.equal (=) Int_emap.empty Int_emap.empty);
  assert (Int_emap.compare compare Int_emap.empty Int_emap.empty = 0);
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
    assert_equal_int ~msg:"cardinal"
		     (Int_map.cardinal m) (Int_emap.cardinal em);
    assert_equal ~msg:"min binding" (Int_emap.min_binding em)
		 (Int_emap.get_binding 0 em);
    assert_equal ~msg:"max binding" (Int_emap.max_binding em)
		 (Int_emap.get_binding (Int_emap.cardinal em - 1) em);
    for i = 0 to Int_emap.cardinal em - 1 do
      let k, _ = Int_emap.get_binding i em in
      let pres, pos = Int_emap.locate k em in
      assert pres;
      assert_equal_int ~msg:"locate (get i em)" i pos
    done;
    test_equal ();
    test_cut ();
    test_alg ()
  done
