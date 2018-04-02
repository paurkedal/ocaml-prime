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

let pow b n =
  if n < 0 then failwith "Prime_int32.pow" else
  let rec loop b n acc =
    if n = 0 then acc else
    loop Int32.(mul b b) (n lsr 1)
         (if n land 1 = 0 then acc else Int32.(mul b acc)) in
  loop b n 1l

let bitcount n =
    Prime_int.bitcount31 (Int32.to_int n)
  + (if n < 0l then 1 else 0)
