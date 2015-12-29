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

let fdiv x y =
  if (x >= 0) = (y >= 0) then x / y
			 else (x - y + 1) / y

let fmod x y =
  if (x >= 0) = (y >= 0) then x mod y
			 else (x - y + 1) mod y + y - 1

let signed_width =
  let rec loop i x =
    let x' = x lsl 1 lor 1 in
    if x' = x then i else loop (succ i) x' in
  loop 0 0

let bitcount16 n =
  let n = (n land 0x5555) + (n lsr 1 land 0x5555) in
  let n = (n land 0x3333) + (n lsr 2 land 0x3333) in
  let n = (n land 0x0f0f) + (n lsr 4 land 0x0f0f) in
  let n = (n land 0x00ff) + (n lsr 8 land 0x00ff) in
  n

let rec bitcount n = if n = 0 then 0 else bitcount16 n + bitcount (n lsr 16)

let rec floor_log2_loop j n l =
  if j = 0 then (assert (n = 1); l) else
  if n lsr j = 0 then floor_log2_loop (j / 2) n l else
  floor_log2_loop (j / 2) (n lsr j) (l + j)

let floor_log2 n =
  if n <= 0 then invalid_arg "floor_log2 on non-positive argument." else
  floor_log2_loop 32 n 0 (* supports up to 64 bits *)

let ceil_log2 n =
  if n <= 0 then invalid_arg "ceil_log2 on non-positive argument." else
  if n = 1 then 0 else
  floor_log2_loop 32 (n - 1) 0 + 1

let fold_to f n acc =
  let rec loop i acc =
    if i = n then acc else
    loop (i + 1) (f i acc) in
  if n < 0 then invalid_arg "Prime_int.fold_to: Negative exponent." else
  loop 0 acc
