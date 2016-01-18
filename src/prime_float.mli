(* Copyright (C) 2016  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Floating point functions. *)

val e : float
(** [e] is the Euler's number [e]. *)

val pi : float
(** [pi] is the well-known number [π]. *)

val sign : float -> float
(** [sign x] is [-1.0], [0.0], or [1.0] where [x] is negative, zero, or
    positive, respectively. *)

val round : float -> float
(** [round x] is the whole number closest to [x], rounding to zero in case of
    a tie. *)

val to_fraction : ?max_denom: int -> float -> int * int
(** [to_fraction x] returns [(n, d)] such that [n/d] is a best rational
    approximation of [x].  The result is based on the continued fraction of
    the absolute value, with sign copied to the numerator.

    @param max_denom The maximum denominator to return.  The default is
    currenty 2{^30} to account for rounding errors and the width of [int] on
    32 bit platforms, but it may be tuned in future versions. *)
