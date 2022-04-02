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

module Infix = struct
  let ( + ) = Int32.add
  let ( - ) = Int32.sub
  let ( * ) = Int32.mul
  let ( lsr ) = Int32.shift_right_logical
  let ( land ) = Int32.logand
end

let pow b n =
  if n < 0 then failwith "Prime_int32.pow" else
  let rec loop b n acc =
    if n = 0 then acc else
    loop Int32.(mul b b) (n lsr 1)
         (if n land 1 = 0 then acc else Int32.(mul b acc)) in
  loop b n 1l

let bitcount =
  (* Cf. https://en.wikipedia.org/wiki/Hamming_weight *)
  let m1 = 0x55555555l in
  let m2 = 0x33333333l in
  let m4 = 0x0f0f0f0fl in
  let h01 = 0x01010101l in
  fun x ->
    let open Infix in
    let x = x - (x lsr 1 land m1) in
    let x = (x land m2) + (x lsr 2 land m2) in
    let x = (x + (x lsr 4)) land m4 in
    Int32.to_int ((x * h01) lsr 24)
