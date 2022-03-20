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

open Array

let sample f xa = init xa f

let fmap f xa =
  let n = length xa in
  let rec fill_phase ya i j =
    if i = n then (if j = Array.length ya then ya else Array.sub ya 0 j) else
    match f xa.(i) with
    | None -> fill_phase ya (i + 1) j
    | Some y -> ya.(j) <- y; fill_phase ya (i + 1) (j + 1) in
  let rec skip_phase i =
    if i = n then [||] else
    match f xa.(i) with
    | None -> skip_phase (i + 1)
    | Some y -> fill_phase (Array.make (n - i) y) (i + 1) 1 in
  skip_phase 0

let filter_map = fmap

(* TODO: Optimise. *)
let filter f = fmap (fun x -> if f x then Some x else None)

let fold f xa accu =
  let accu_r = ref accu in
  for i = 0 to length xa - 1 do
    accu_r := f xa.(i) !accu_r
  done;
  !accu_r

let foldi f xa accu =
  let accu_r = ref accu in
  for i = 0 to length xa - 1 do
    accu_r := f i xa.(i) !accu_r
  done;
  !accu_r

let for_all f xa =
  let n = length xa in
  let rec ok_from i = i >= n || f xa.(i) && ok_from (i + 1) in
  ok_from 0

let exists f xa =
  let n = length xa in
  let rec ok_from i = i < n && (f xa.(i) || ok_from (i + 1)) in
  ok_from 0

let count f xa =
  let n = length xa in
  let rec loop c i =
    if i = n then c else
    loop (if f xa.(i) then c + 1 else c) (i + 1) in
  loop 0 0

let find_map f xa =
  let n = length xa in
  let rec loop i =
    if i = n then None else
    (match f xa.(i) with
     | None -> loop (i + 1)
     | y -> y)
  in
  loop 0

let search = find_map

let slice i j xa = sub xa i (j - i)
