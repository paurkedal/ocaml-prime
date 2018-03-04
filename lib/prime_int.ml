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

let sign n = compare n 0

let fdiv x y =
  if (x >= 0) = (y >= 0) then x / y
                         else (x - y + 1) / y

let fmod x y =
  if (x >= 0) = (y >= 0) then x mod y
                         else (x - y + 1) mod y + y - 1

let cdiv x y =
  if (x >= 0) = (y >= 0) then (x + y - 1) / y
                         else x / y

let cmod x y =
  if (x >= 0) = (y >= 0) then (x + y - 1) mod y - y + 1
                         else x mod y

(* Based on "Binary GCD algoritm" from Wikipedia. *)
let gcd u v =
  if u = 0 then abs v else
  if v = 0 then abs u else

  let rec common_shift u v sh =
    if (u lor v) land 1 <> 0 then (u, v, sh) else
    common_shift (u lsr 1) (v lsr 1) (succ sh) in

  let rec skip_shift u =
    if u land 1 <> 0 then u else
    skip_shift (u lsr 1) in

  let rec common_divisor u v =
    if v = 0 then u else
    let v = skip_shift v in
    if u <= v then common_divisor u (v - u)
              else common_divisor v (u - v) in

  let u, v, p = common_shift (abs u) (abs v) 0 in
  common_divisor (skip_shift u) v lsl p

let delta n m = if n = m then 1 else 0

let fact n =
  let rec loop n acc =
    if n <= 1 then acc else
    let acc' = n * acc in
    if acc' >= acc then loop (n - 1) (n * acc) else
    failwith "Prime_int.fact: overflow" in
  if n >= 0 then loop n 1 else
  invalid_arg "Prime_int.fact: negative"

let binom n k =
  let rec loop n k i acc =
    if i >= k then acc else
    let acc' = acc * (n - i) / (i + 1) in
    if acc' >= acc then loop n k (i + 1) acc' else
    let acc' = acc / (i + 1) * (n - i) + acc mod (i + 1) * (n - i) / (i + 1) in
    if acc' >= acc then loop n k (i + 1) acc' else
    failwith "Prime_int.binom: overflow" in
  if k < n - k then loop n k 0 1 else loop n (n - k) 0 1

let signed_width =
  let rec loop i x =
    let x' = x lsl 1 lor 1 in
    if x' <= x then i else loop (succ i) x' in
  loop 0 0

let bitcount16 n =
  let n = (n land 0x5555) + (n lsr 1 land 0x5555) in
  let n = (n land 0x3333) + (n lsr 2 land 0x3333) in
  let n = (n land 0x0f0f) + (n lsr 4 land 0x0f0f) in
  let n = (n land 0x00ff) + (n lsr 8 land 0x00ff) in
  n

let bitcount31 n =
  let n = (n land 0x55555555) + (n lsr  1 land 0x15555555(*sic*)) in
  let n = (n land 0x33333333) + (n lsr  2 land 0x33333333) in
  let n = (n land 0x0f0f0f0f) + (n lsr  4 land 0x0f0f0f0f) in
  let n = (n land 0x00ff00ff) + (n lsr  8 land 0x00ff00ff) in
  let n = (n land 0x0000ffff) + (n lsr 16 land 0x0000ffff) in
  n

let rec bitcount n = if n = 0 then 0 else bitcount31 n + bitcount (n lsr 31)

let floor_log2_halfwidth = if signed_width <= 32 then 16 else 32

let rec floor_log2_loop j n l =
  if j = 0 then (assert (n = 1); l) else
  if n lsr j = 0
    then floor_log2_loop (j / 2) n l
    else floor_log2_loop (j / 2) (n lsr j) (l + j)

let floor_log2 n =
  if n <= 0 then invalid_arg "floor_log2 on non-positive argument." else
  floor_log2_loop floor_log2_halfwidth n 0

let ceil_log2 n =
  if n <= 0 then invalid_arg "ceil_log2 on non-positive argument." else
  if n = 1 then 0 else
  floor_log2_loop floor_log2_halfwidth (n - 1) 0 + 1

let fold_to f n acc =
  let rec loop i acc =
    if i = n then acc else
    loop (i + 1) (f i acc) in
  if n < 0 then invalid_arg "Prime_int.fold_to: Negative exponent." else
  loop 0 acc
