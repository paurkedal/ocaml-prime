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

let bitcount16 n =
  let n = (n land 0x5555) + (n lsr 1 land 0x5555) in
  let n = (n land 0x3333) + (n lsr 2 land 0x3333) in
  let n = (n land 0x0f0f) + (n lsr 4 land 0x0f0f) in
  let n = (n land 0x00ff) + (n lsr 8 land 0x00ff) in
  n

let rec bitcount n = if n = 0 then 0 else bitcount16 n + bitcount (n lsr 16)

let floor_log2 n =
  let rec loop j n l =
    if j = 0 then (assert (n = 1); l) else
    if n lsr j = 0 then loop (j / 2) n l else
    loop (j / 2) (n lsr j) (l + j) in
  if n <= 0 then invalid_arg "floor_log2 on non-positive argument." else
  loop 32 n 0 (* supports up to 64 bits *)
