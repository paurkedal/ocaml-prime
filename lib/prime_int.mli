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

(** Integer functions. *)


val sign : int -> int
(** [sign n] is [compare n 0]. *)

val delta : int -> int -> int
(** [delta n m] is [1] if [n = m], [0] otherwise. *)


(** {2 Arithmetic} *)

val pow : int -> int -> int
(** [pow b n] is [b] raised to [n].
    @raise Failure if [n < 0]. *)

val fdiv : int -> int -> int
(** [fdiv n m] is the quotient of floored division, [⌊n / m⌋]. *)

val fmod : int -> int -> int
(** [fmod n m] is the remainder of floored division, [n / m - ⌊n / m⌋]. *)

val cdiv : int -> int -> int
(** [cdiv n m] is the quotient of ceiled division, [⌈n / m⌉]. *)

val cmod : int -> int -> int
(** [cmod n m] is the remainder of ceiled division, [n / m - ⌈n / m⌉]. *)

val gcd : int -> int -> int
(** [gcd n m] is the greatest common divisor of [n] and [m]. *)


(** {2 Combinatorics} *)

val fact : int -> int
(** [fact n] is the factorial of [n] representing the number of ways to order
    [n] distinct elements. *)

val binom : int -> int -> int
(** [binom n k] is the binominal coefficient corresponding to the number of ways
    to choosing [k] elements out of [n].

    @raise Failure if the result cannot be represented as an [int]. *)

(** {2 Bitwise Operations} *)

val signed_width : int
(** The number of non-sign bits of an [int]. *)

val bitcount : int -> int
(** [bitcount n] is the number of ones in the binary representation of [n]. *)

val floor_log2 : int -> int
(** [floor_log2 n] is [⌊log₂ n⌋]. *)

val ceil_log2 : int -> int
(** [ceil_log2 n] is [⌈log₂ n⌉]. *)


(** {2 Iteration} *)

val fold_to : (int -> 'a -> 'a) -> int -> 'a -> 'a
(** [fold_to f n] returns [f (n - 1) ∘ ⋯ ∘ f 0].
    @raise Invalid_argument if [n < 0]. *)

val range : start: int -> ?stop: int -> ?step: int -> unit -> int Seq.t


(**/**)
val bitcount16 : int -> int
val bitcount31 : int -> int
