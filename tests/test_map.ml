(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

module Int_map = Prime_map.Make (struct type t = int let compare = compare end)

let run () =
  let m = Prime_int.fold_to (fun k -> Int_map.add k (-k)) 100 Int_map.empty in
  let m2 = Int_map.filter (fun k _ -> k mod 2 = 0) m in
  let m3 = Int_map.filter (fun k _ -> k mod 3 = 0) m in
  let a, b, ab = Int_map.split_union (fun _ i j -> (i, j)) m2 m3 in
  let a' = Int_map.compl m3 m2 in
  let b' = Int_map.compl m2 m3 in
  let ab' = Int_map.map2t (fun i j -> (i, j)) m2 m3 in
  assert (Int_map.equal (=) a a');
  assert (Int_map.equal (=) b b');
  assert (Int_map.equal (=) ab ab')
