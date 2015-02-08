(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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

open Unprime
open Unprime_list

module L = Prime_enumlist
module Int_order = struct type t = int let compare = compare end
module Int_set = Prime_enumset.Make (Int_order)

let permute_array a =
  for i = 1 to Array.length a - 1 do
    let j = Random.int i in
    let tmp = a.(j) in
    a.(j) <- a.(i);
    a.(i) <- tmp
  done

let test_push () =
  let n = Random.int (1 lsl Random.int 10) in
  let xs = List.sample (fun _ -> Random.int n) n in
  let l0 = List.fold L.push_last xs L.empty in
  let l1 = List.fold_right L.push_first xs L.empty in
  assert (L.equal (=) l0 l1);
  let xs0 = L.elements l0 in
  assert (xs0 = xs)

let test_insert_delete () =
  let n0 = Random.int (1 lsl Random.int 10) in
  let n1 = Random.int (1 lsl Random.int 10) in
  let a01 = Prime_array.sample ident (n0 + n1) in
  permute_array a01;
  let a0 = Prime_array.slice 0 n0 a01 in
  let a1 = Prime_array.slice n0 (n0 + n1) a01 in

  let ins x (s, l) =
    let _, pos = Int_set.locate x s in
    let s' = Int_set.add x s in
    let l' = L.insert pos x l in
    (s', l') in

  let del x (s, l) =
    let _, pos = Int_set.locate x s in
    let s' = Int_set.remove x s in
    let l' = L.delete pos l in
    (s', l') in

  permute_array a0;
  let s0, l0 = Prime_array.fold ins a0 (Int_set.empty, L.empty) in
  assert (Int_set.cardinal s0 = L.length l0);
  assert (Int_set.elements s0 = L.elements l0);

  permute_array a1;
  let s01, l01 = Prime_array.fold ins a1 (s0, l0) in
  assert (Int_set.cardinal s01 = L.length l01);
  assert (Int_set.elements s01 = L.elements l01);

  permute_array a0;
  let s1, l1 = Prime_array.fold del a0 (s01, l01) in
  assert (Int_set.cardinal s1 = L.length l1);
  assert (Int_set.elements s1 = L.elements l1)

let run () =
  assert (L.is_empty L.empty);
  assert (L.length L.empty = 0);
  assert (not (L.is_empty (L.singleton 0)));
  assert (L.length (L.singleton 0) = 1);
  assert (L.get 0 (L.singleton 3) = 3);
  for round = 0 to 999 do
    test_push ();
    test_insert_delete ()
  done