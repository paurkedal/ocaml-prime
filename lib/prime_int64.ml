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

let pow b n =
  if n < 0 then failwith "Prime_int64.pow" else
  let rec loop b n acc =
    if n = 0 then acc else
    loop Int64.(mul b b) (n lsr 1)
         (if n land 1 = 0 then acc else Int64.(mul b acc)) in
  loop b n 1L

let bitcount n =
    Prime_int.bitcount16 (Int64.to_int n)
  + Prime_int.bitcount31 (Int64.to_int (Int64.shift_right_logical n 16))
  + Prime_int.bitcount31 (Int64.to_int (Int64.shift_right_logical n 47))
