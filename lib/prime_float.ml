(* Copyright (C) 2016--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

let e = exp 1.0

let pi = 4.0 *. atan 1.0

let sign x =
  if x < 0.0 then -1.0 else
  if x > 0.0 then  1.0 else 0.0

let round x = if x >= 0.0 then ceil (x -. 0.5) else floor (x +. 0.5)

(* We have 53 bits, but there may be significant noise in the last bits after
 * repeated reciprocals. *)
let default_max_denom = 1 lsl 30

let to_fraction =
  if max_int lsr 31 < 1024 then
    (* Use floating point arithmetic on 32 bit platforms. *)
    begin fun ?(max_denom = default_max_denom) x ->
      let max_denom = float_of_int max_denom in
      let rec loop n' n'' d' d'' x =
        let c, a = modf x in
        let n = a *. n' +. n'' in
        let d = a *. d' +. d'' in
        if d > max_denom then (n', d') else
        if c <= epsilon_float then (n, d) else
        loop n n' d d' (1.0 /. c) in
      if x >= 0.0 then
        let n, d = loop 1. 0. 0. 1. x in
        (int_of_float n, int_of_float d)
      else
        let n, d = loop 1. 0. 0. 1. (-. x) in
        (- int_of_float n, int_of_float d)
    end
  else
    (* Accumulate with integers on 64 bit platforms. *)
    begin fun ?(max_denom = default_max_denom) x ->
      let rec loop n' n'' d' d'' x =
        let c, a = modf x in
        let a = int_of_float a in
        let n = a * n' + n'' in
        let d = a * d' + d'' in
        if d < 0 || d > max_denom then (n', d') else
        if c <= epsilon_float then (n, d) else
        loop n n' d d' (1.0 /. c) in
      if x >= 0.0 then
        loop 1 0 0 1 x
      else
        let n, d = loop 1 0 0 1 (-. x) in
        (- n, d)
    end
