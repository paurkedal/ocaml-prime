(* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
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

(** Amendment to [List].  This structure does not include the original
    functions.  For a full replacement include {!Unprime_list}. *)

val sample : (int -> 'a) -> int -> 'a list
(** [sample f n] returns [[f 0; …; f (n - 1)]]. *)

val push : 'a -> 'a list -> 'a list
(** [push] is a function alias for the [::] operator, convenient for passing
    as a function argument. *)

val search : ('a -> 'b option) -> 'a list -> 'b option
(** [search f xs] returns the first element of [map f xs] which is different
    from [None] if it exists, otherwise it returns [None]. *)

val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
(** [fold f [x₁; …; xₙ]] returns the composition [f xₙ ∘ ⋯ ∘ f x₁].  This is
    [fold_left] with parameters reordered to make it more composable. *)

val fold_zip : ?trunc: bool ->
	       ('a * 'b -> 'c -> 'c) -> 'a list * 'b list -> 'c -> 'c
(** [fold_zip f] is an optimization of [fold f ∘ zip]. *)

val zip : ?trunc: bool -> 'a list * 'b list -> ('a * 'b) list
(** [zip ([x₁; …; xₙ], [y₁; …; yₙ])] returns [[(x₁, y₁); …; (xₙ, yₙ)]].
    If [~trunc:true] is passed to [zip], then either of the list arguments
    will be truncated to the shorter one, othewise [Invalid_argument] is
    raised if the list have different length.  This is a tail-recursive
    variant of [combine]. *)

val unzip : ('a * 'b) list -> 'a list * 'b list
(** [unzip [(x₁, y₁); …; (xₙ, yₙ)]] returns [([x₁; …; xₙ], [y₁; …; yₙ])].
    This is a tail-recursive variant of [split]. *)

val drop_while : ('a -> bool) -> 'a list -> 'a list
(** [drop_while f xs] returns the longest suffix of [xs] which does not start
    with an element on which [f] is true. *)

val take_while : ('a -> bool) -> 'a list -> 'a list
(** [take_while f xs] returns the longest prefix of [xs] containing only
    elements which [f] maps to true. *)
