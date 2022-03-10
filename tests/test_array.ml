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

open Unprime_array

let is_even x = x mod 2 = 0
let half_even x = if x mod 2 = 0 then Some (x / 2) else None

let run () =
  let a = [|4; 1; 2; 3|] in
  let b = [|1; 2; 4; 3; 6|] in
  let c = [|3; 7|] in
  assert (Array.filter is_even a = [|4; 2|]);
  assert (Array.filter is_even b = [|2; 4; 6|]);
  assert (Array.filter_map half_even a = [|2; 1|]);
  assert (Array.filter_map half_even b = [|1; 2; 3|]);
  assert (Array.fold (+) a 10 = 20);
  assert (Array.foldi (fun i x acc -> x lsl i + acc) a 10 = 48);
  assert (Array.for_all ((<) 0) a);
  assert (not (Array.for_all ((<) 1) a));
  assert (Array.exists ((=) 2) a);
  assert (not (Array.exists ((=) 5) a));
  assert (Array.count is_even b = 3);
  assert (Array.search half_even b = Some 1);
  assert (Array.search half_even c = None);
  assert (Array.slice 2 2 b = [||]);
  assert (Array.slice 2 4 b = [|4; 3|]);
  ()
