(* Copyright (C) 2013--2018  Petter A. Urkedal <paurkedal@gmail.com>
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
open Unprime_list

module Int_order = struct type t = int let compare = compare end
module Int_set = Set.Make (Int_order)
module Int_eset = Prime_enumset.Make (Int_order)

let test_equal () =
  let es0 = Prime_array.sample (fun _ -> Random.int 40) 40 in
  let es1 = Array.copy es0 in
  Array.sort compare es1;
  let s0 = Array.fold Int_eset.add es0 Int_eset.empty in
  let s1 = Array.fold Int_eset.add es1 Int_eset.empty in
  assert (Int_eset.equal s0 s1);
  assert (Int_eset.compare s0 s1 = 0);
  let s2 = Int_eset.remove es0.(Random.int (Array.length es0)) s0 in
  assert (not (Int_eset.equal s0 s2));
  assert (not (Int_eset.equal s1 s2));
  let c02 = Int_eset.compare s0 s2 in
  let c12 = Int_eset.compare s1 s2 in
  let c20 = Int_eset.compare s2 s0 in
  let c21 = Int_eset.compare s2 s1 in
  assert (c02 <> 0);
  assert (c12 <> 0);
  assert (c20 = - c02);
  assert (c21 = - c12)

let test_cut () =
  let n = 40 in
  let es = Prime_array.sample (fun _ -> Random.int n) n in
  let s = Array.fold Int_eset.add es Int_eset.empty in
  Array.sort compare es;
  let i_cut = Random.int (Array.length es) in
  let e_cut = es.(i_cut) in
  let sL, sR =
    Array.fold
      (fun e (sL, sR) ->
        if e < e_cut then (Int_eset.add e sL, sR) else
        if e > e_cut then (sL, Int_eset.add e sR) else
        (sL, sR))
      es (Int_eset.empty, Int_eset.empty) in
  let pres, sL', sR' = Int_eset.cut_element es.(i_cut) s in
  assert pres;
  assert (Int_eset.equal sL sL');
  assert (Int_eset.equal sR sR')

let rec uniq = function
  | x0 :: x1 :: xs when x0 = x1 -> uniq (x1 :: xs)
  | x0 :: xs -> x0 :: uniq xs
  | [] -> []

let test_alg () =
  let nA = Random.int 30 in
  let nB = Random.int 30 in
  let n = max nA nB in
  let esA = Prime_array.sample (fun _ -> Random.int n) nA in
  let esB = Prime_array.sample (fun _ -> Random.int n) nB in
  let sA = Array.fold Int_eset.add esA Int_eset.empty in
  let sB = Array.fold Int_eset.add esB Int_eset.empty in
  let sAuB = Int_eset.union sA sB in
  assert (Int_eset.subset sA sAuB);
  assert (Int_eset.subset sB sAuB);
  assert (not (Int_eset.subset sAuB sA) || Int_eset.equal sA sAuB);
  assert (not (Int_eset.subset sAuB sB) || Int_eset.equal sB sAuB);
  let sAuB' = Array.fold Int_eset.add esB sA in
  assert (Int_eset.equal sAuB sAuB');
  let sAnB = Int_eset.inter sA sB in
  assert (Int_eset.subset sAnB sA);
  assert (Int_eset.subset sAnB sB);
  assert (not (Int_eset.subset sA sAnB) || Int_eset.equal sA sAnB);
  assert (not (Int_eset.subset sB sAnB) || Int_eset.equal sB sAnB);
  let sAnB' =
    Array.fold
      (fun i -> if Int_eset.mem i sA then Int_eset.add i else ident)
      esB Int_eset.empty in
  assert (Int_eset.equal sAnB sAnB');
  let sAnB'' = Int_eset.filter (fun e -> Int_eset.mem e sA) sB in
  assert (Int_eset.equal sAnB sAnB'');
  let sAcB = Int_eset.compl sA sB in
  assert (Int_eset.subset sAcB sB);
  let sAcB' = Int_eset.filter (fun e -> not (Int_eset.mem e sA)) sB in
  assert (Int_eset.equal sAcB sAcB')

let test_elements () =
  let n = 1 lsl Random.int 8 in
  let es_init = Prime_list.sample (fun _ -> Random.int n) n in
  let s = List.fold Int_eset.add es_init Int_eset.empty in
  let es = Int_eset.elements s in
  assert (uniq (List.sort compare es_init) = es);
  let s' = Int_eset.of_ordered_elements es in
  assert (Int_eset.equal s s')

let run () =
  assert (Int_eset.equal Int_eset.empty Int_eset.empty);
  assert (Int_eset.compare Int_eset.empty Int_eset.empty = 0);
  for _ = 0 to 999 do
    let rec populate imax n s es =
      if n < 0 then (s, es) else
      let i = Random.int imax in
      let j = Random.int imax in
      let es' = Int_eset.remove i es in
      let es'' = Int_eset.add j es' in
      assert (not (Int_eset.mem i es'));
      assert (Int_eset.mem j es'');
      populate imax (n - 1) (Int_set.add j (Int_set.remove i s)) es'' in
    let n = Random.int (1 lsl Random.int 10) + 1 in
    let s, es = populate n n Int_set.empty Int_eset.empty in
    assert_equal_int ~msg:"cardinality using fold" (Int_set.cardinal s)
                     (Int_eset.fold (fun _ -> (+) 1) es 0);
    assert_equal_int ~msg:"cardinal"
                     (Int_set.cardinal s) (Int_eset.cardinal es);
    assert_equal ~msg:"elements" (Int_set.elements s)
                 (List.rev (Int_eset.fold List.cons es []));
    assert_equal_int ~msg:"min element" (Int_eset.min_elt_exn es)
                     (Int_eset.get es 0);
    assert_equal_int ~msg:"max element" (Int_eset.max_elt_exn es)
                     (Int_eset.get es (Int_eset.cardinal es - 1));
    for i = 0 to Int_eset.cardinal es - 1 do
      let e = Int_eset.get es i in
      let pres, pos = Int_eset.locate e es in
      assert pres;
      assert_equal_int ~msg:"locate (get i es)" i pos
    done;
    test_equal ();
    test_cut ();
    test_alg ();
    test_elements ()
  done
