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


val sign : int -> int
(** [sign n] is [compare n 0]. *)

val delta : int -> int -> int
(** [delta n m] is [1] if [n = m], [0] otherwise. *)


(** {2 Division} *)

val fdiv : int -> int -> int
(** [fdiv n m] is [⌊n / m⌋], the quotient of floored division. *)

val fmod : int -> int -> int
(** [fmod n m] is [n / m - ⌊n / m⌋], the remainder of floored division. *)

val cdiv : int -> int -> int
(** [cdiv n m] is [⌈n / m⌉], the quotient of ceiled division. *)

val cmod : int -> int -> int
(** [cmod n m] is [n / m - ⌈n / m⌉], the remainder of ceiled division. *)

val gcd : int -> int -> int
(** [gcd n m] is the greatest common divisor of [n] and [m]. *)


(** {2 Bitwise Operations} *)

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


(** {2 Iteration} *)

val fold_to : (int -> 'a -> 'a) -> int -> 'a -> 'a
(** [fold_to f n] returns [f (n - 1) ∘ ⋯ ∘ f 0].
    @raise Invalid_argument if [n < 0]. *)
