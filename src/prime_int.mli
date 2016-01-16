(* Copyright (C) 2013--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Integer functions. *)

val fdiv : int -> int -> int
(** [fdiv x y] is the quotient of the floored division of [x] by [y]. *)

val fmod : int -> int -> int
(** [fmod x y] is the remainder of the floored division of [x] by [y]. *)

val gcd : int -> int -> int
(** [gcd x y] is the greatest common divisor of [x] and [y]. *)

val signed_width : int
(** The number of bits which can be stored in a signed integer. *)

val bitcount : int -> int
(** [bitcount n] is the number of ones in the binary representation of [n]. *)

val bitcount16 : int -> int
(** [bitcount16 n] is the number of ones in the lowest 16 bits of [n]. *)

val floor_log2 : int -> int
(** [floor_log2 n] is [⌊log₂ n⌋]. *)

val ceil_log2 : int -> int
(** [ceil_log2 n] is [⌈log₂ n⌉]. *)

val fold_to : (int -> 'a -> 'a) -> int -> 'a -> 'a
(** [fold_to f n] returns [f (n - 1) ∘ ⋯ ∘ f 0].
    @raise Invalid_argument if [n < 0]. *)
