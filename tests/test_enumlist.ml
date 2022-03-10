(* Copyright (C) 2015--2022  Petter A. Urkedal <paurkedal@gmail.com>
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
  let l2 = L.of_list xs in
  assert (L.equal (=) l0 l1);
  assert (L.equal (=) l0 l2);
  let xs0 = L.to_list l0 in
  assert (xs0 = xs);
  let l3 = L.push_last 0 l2 in
  assert (L.compare compare l2 l3 = -1);
  assert (L.compare compare l3 l2 = 1);
  assert (L.equal (=) l3 l2 = false);
  if n > 0 then begin
    let l4 = L.push_first (n + 1) (snd (L.pop_first_exn l2)) in
    assert (L.compare compare l2 l4 = -1);
    assert (L.compare compare l4 l2 = 1);
    assert (L.equal (=) l2 l4 = false);
    let l5 = L.push_first (-1) (snd (L.pop_first_exn l2)) in
    assert (L.compare compare l2 l5 = 1);
    assert (L.compare compare l5 l2 = -1);
    assert (L.equal (=) l2 l5 = false)
  end

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
  assert (Int_set.elements s0 = L.to_list l0);

  permute_array a1;
  let s01, l01 = Prime_array.fold ins a1 (s0, l0) in
  assert (Int_set.cardinal s01 = L.length l01);
  assert (Int_set.elements s01 = L.to_list l01);

  permute_array a0;
  let s1, l1 = Prime_array.fold del a0 (s01, l01) in
  assert (Int_set.cardinal s1 = L.length l1);
  assert (Int_set.elements s1 = L.to_list l1)

let test_cutting () =
  let n = Random.int 100 in
  let m = Random.int (n + 1) in
  let l = L.sample (fun i -> 2*i + 1) n in
  let l1, l2 = L.cut m l in
  assert (L.length l1 = m);
  assert (L.length l2 = n - m);
  assert (L.equal (=) l (L.cat l1 l2))

let test_iteration () =
  let n = Random.int 100 in
  let l = L.sample (fun i -> 2*i + 1) n in
  assert (L.length l = n);
  for i = 0 to n - 1 do
    assert (L.get l i = 2*i + 1)
  done;
  L.iteri (fun i x -> assert (x = 2*i + 1)) l;
  let m = Random.int (2*n + 2) in
  let l1 =
    L.filter_mapi
      (fun i x -> assert (x = 2*i + 1); if x <= m then Some (i + x) else None) l
  in
  let l2 = l |> L.filter (fun x -> x <= m)
             |> L.mapi (fun i x -> assert (x = 2*i + 1); i + x) in
  assert (L.equal (=) l1 l2);
  let y = L.foldi (fun i x acc -> assert (x = 2*i + 1);
                                  assert (acc = 2*i - 1); acc + 2) l (-1) in
  assert (y = 2*n - 1)

let run () =
  assert (L.is_empty L.empty);
  assert (L.length L.empty = 0);
  assert (not (L.is_empty (L.singleton 0)));
  assert (L.length (L.singleton 0) = 1);
  assert (L.get (L.singleton 3) 0 = 3);
  for _ = 0 to 999 do
    test_push ();
    test_insert_delete ();
    test_cutting ();
    test_iteration ()
  done
