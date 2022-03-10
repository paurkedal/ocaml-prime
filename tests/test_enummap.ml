(* Copyright (C) 2013--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

open OUnit
open Utils
open Unprime
open Unprime_array
open Unprime_list
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

let test_update () =
  let n = 40 in
  let rec loop i m = if i < n then begin
    let k = Random.int n in
    let e' = if Random.bool () then None else Some (Random.int n) in
    let aux = function
     | None -> assert (not (Int_emap.mem k m)); e'
     | Some e -> assert (Int_emap.find k m = e); e'
    in
    let m' = Int_emap.update k aux m in
    let m'' = match e' with
     | None -> Int_emap.remove k m
     | Some e' -> Int_emap.add k e' m
    in
    assert (Int_emap.equal (=) m' m'');
    loop (i + 1) m'
  end in
  loop 0 Int_emap.empty

let test_pop_remove () =
  let n_max = 1 lsl Random.int 6 in
  let m = random_emap n_max in
  for _ = 0 to 15 do
    let k = Random.int n_max in
    if not (Int_emap.mem k m) then begin
      assert (Int_emap.pop k m = None);
      let m' = Int_emap.add k (2*k + 1) m in
      assert (Int_emap.equal (=) (Int_emap.remove k m') m);
      match Int_emap.pop k m' with
      | None -> assert false
      | Some (e, m'') ->
        assert (e = 2*k + 1);
        assert (Int_emap.equal (=) m m'')
    end
  done

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
  let e_opt, mL', mR' = Int_emap.cut_binding es.(i_cut) m in
  assert (e_opt = Some e_cut);
  assert (Int_emap.equal (=) mL mL');
  assert (Int_emap.equal (=) mR mR')

let test_alg () =
  let n_max = 1 lsl Random.int 8 in
  let mA = random_emap n_max in
  let mB = random_emap n_max in
  let mAnB = Int_emap.finter (fun _ x y -> Some (max x y)) mA mB in
  let mAnB' = Int_emap.merge (fun _ -> Option.inter max) mA mB in
  let mAnB'' = Int_emap.filter (fun k _ -> Int_emap.mem k mB) mA in
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
          Int_emap.cardinal mB - Int_emap.cardinal mAnB);
  let pB = Int_emap.merge
            (fun _ eA_opt eB_opt ->
              match eA_opt, eB_opt with
              | None, None -> assert false
              | None, Some eB -> Some (Some eB)
              | Some _, None -> Some None
              | Some eA, Some eB when eA = eB -> None
              | Some _, Some eB -> Some (Some eB))
            mA mB in
  let mB' = Int_emap.fpatch (fun _ e_opt _ -> e_opt) pB mA in
  assert (Int_emap.equal (=) mB mB')

let test_bindings () =
  let n_max = 1 lsl Random.int 8 in
  let m = random_emap n_max in
  let kvs = Int_emap.bindings m in
  let m' = List.fold (uncurry Int_emap.add) kvs Int_emap.empty in
  assert (Int_emap.equal (=) m m');
  let m'' = Int_emap.of_ordered_bindings kvs in
  assert (Int_emap.equal (=) m m'');
  for _ = 0 to 9 do
    let i, j = Random.int n_max, Random.int n_max in
    let i, j = if i < j then (i, j) else (j, i) in
    let where e = if e < i then -1 else if e > j then 1 else 0 in
    let es_ij = Int_emap.asc_bindings ~where m |> List.of_seq in
    let es_ij_rev = Int_emap.dsc_bindings ~where m |> List.of_seq in
    let m_ij = Int_emap.filter (fun k _ -> i <= k && k <= j) m in
    assert (es_ij = Int_emap.bindings m_ij);
    assert (es_ij = List.rev es_ij_rev)
  done

let run () =
  assert (Int_emap.equal (=) Int_emap.empty Int_emap.empty);
  assert (Int_emap.compare compare Int_emap.empty Int_emap.empty = 0);
  for _ = 0 to 999 do
    let rec populate imax n m em =
      if n < 0 then (m, em) else
      let i = Random.int imax in
      let j = Random.int imax in
      let em' = Int_emap.remove i em in
      let em'' = Int_emap.add j (j + 1) em' in
      assert (not (Int_emap.mem i em'));
      assert (Int_emap.mem j em'');
      assert_equal (Int_emap.find j em'') (j + 1);
      populate imax (n - 1) (Int_map.add j (j + 1) (Int_map.remove i m)) em'' in
    let n = Random.int (1 lsl Random.int 10) + 1 in
    let m, em = populate n n Int_map.empty Int_emap.empty in
    assert_equal_int ~msg:"cardinality using fold" (Int_map.cardinal m)
                     (Int_emap.fold (fun _ _ -> (+) 1) em 0);
    assert_equal_int ~msg:"cardinal"
                     (Int_map.cardinal m) (Int_emap.cardinal em);
    assert_equal ~msg:"min binding" (Option.get (Int_emap.min_binding em))
                 (Int_emap.get_binding em 0);
    assert_equal ~msg:"max binding" (Option.get (Int_emap.max_binding em))
                 (Int_emap.get_binding em (Int_emap.cardinal em - 1));
    for i = 0 to Int_emap.cardinal em - 1 do
      let k, _ = Int_emap.get_binding em i in
      let pres, pos = Int_emap.locate k em in
      assert pres;
      assert_equal_int ~msg:"locate (get i em)" i pos
    done;
    test_equal ();
    test_update ();
    test_pop_remove ();
    test_cut ();
    test_alg ();
    test_bindings ()
  done
