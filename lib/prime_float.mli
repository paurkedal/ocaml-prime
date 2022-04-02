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

(** Floating point functions. *)

val e : float
(** [e] is Euler's number {e e}. *)

val pi : float
(** [pi] is the ratio {e π} of the circumference to the diameter of a circle in
    the Euclidean plane.
    @deprecated This is available in the standard library since 4.07.0. *)

val sign : float -> float
(** [sign x] is [-1.0], [0.0], or [1.0] where [x] is negative, zero, or
    positive, respectively.  This is different from
    [Stdlib.Float.copy_sign 1.0], which returns [1.0] and [-1.0] at [0.0] and
    [0.0], respectively. *)

val round : float -> float
[@@deprecated
  "Use Stdlib.Float.round instead, available since OCaml 4.07, \
   but note the opposite behaviour in case of a tie."]
(** [round_nonstd x] is the whole number closest to [x], rounding towards zero
    in case of a tie. *)

val to_fraction : ?max_denom: int -> float -> int * int
(** [to_fraction x] returns [(n, d)] such that [n/d] is a best rational
    approximation of [x].  The result is based on the continued fraction of
    the absolute value, with sign copied to the numerator.

    @param max_denom The maximum denominator to return.  The default is
    currenty 2{^30} to account for rounding errors and the width of [int] on
    32 bit platforms, but it may be tuned in future versions. *)
