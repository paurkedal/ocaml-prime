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

open Array

let sample f xa = init xa f

let fold f xa accu =
  let accu_r = ref accu in
  for i = 0 to length xa - 1 do
    accu_r := f xa.(i) !accu_r
  done;
  !accu_r

let search f xa =
  let n = length xa in
  let rec search_from i =
    if i = n then None else
    match f xa.(i) with
    | None -> search_from (i + 1)
    | y -> y in
  search_from 0

let slice i j xa = sub xa i (j - i)
