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

(** Amendment to the standard library [List] structure.
    
    This structure does not include the original functions.  For a full
    replacement include {!Unprime_list}. *)

val sample : (int -> 'a) -> int -> 'a list
(** [sample f n] returns [[f 0; …; f (n - 1)]]. *)

val push : 'a -> 'a list -> 'a list
(** [push] is a function alias for the [::] operator, convenient for passing
    as a function argument. *)

val count : ('a -> bool) -> 'a list -> int
(** [count f xs] is the number of elements [x] of [xs] for which [f x]
    holds. *)

val search : ('a -> 'b option) -> 'a list -> 'b option
(** [search f xs] returns the first element of [map f xs] which is different
    from [None] or [None] if all elements are [None].  This is an alternative
    to [find] which is easy to nest, e.g. the function
    [Prime_array.search (Prime_option.search (Prime_list.search f))]
    returns the first non-[None] mapping of [f] in an array of optional
    lists. *)


(** {2 Iteration} *)

val fold : ('a -> 'b -> 'b) -> 'a list -> 'b -> 'b
(** [fold f [x₁; …; xₙ]] returns the composition [f xₙ ∘ ⋯ ∘ f x₁].  This is
    [fold_left] with parameters reordered to make it more composable. *)

val filter_map : ('a -> 'b option) -> 'a list -> 'b list
(** [filter_map f xs] is the list of all [y] such that [f x = Some y] for some
    [x] in [xs], and having the same order as the corresponding elemets of
    [xs].  This provides an optimisation of [map Option.get (filter ((<>)
    None) (map f xs))]. *)

val flatten_map : ('a -> 'b list) -> 'a list -> 'b list
(** [flatten_map f xs] is a tail-recursive optimisation of [flatten (map f
    xs)]. *)

(** {2 Iteration over Two Lists} *)

val iter2t : ('a -> 'b -> unit) -> 'a list -> 'b list -> unit
(** [iter2t f [x₁; …; xₙ] [y₁; …; yₘ]] calls [f x₁ y₁; …; f xₗ yₗ] in order,
    where [l = min n m].  This is a truncating variant of [iter2] from the
    standard library. *)

val map2t : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(** [map2t f [x₁; …; xₙ] [y₁; …; yₘ]] returns [[f x₁ y₁; ⋯; f xₗ yₗ]] where
    [l = min n m].  The [t] suffix indicates truncation, otherwise this
    behaves like [map2] from the standard library. *)

val rev_map2t : ('a -> 'b -> 'c) -> 'a list -> 'b list -> 'c list
(** [rev_map2t f] is an optimisation of [rev ∘ map2t f]. *)

val fold2 : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
(** [fold2 f g [x₁; …; xₙ] [y₁; …; yₘ]] returns [f (xₙ, yₙ) ∘ ⋯ ∘ f (x₁, y₁)]
    and [n = m] and raises [Invalid_argument] if [n ≠ m]. *)

val fold2t : ('a -> 'b -> 'c -> 'c) -> 'a list -> 'b list -> 'c -> 'c
(** [fold2t f g [x₁; …; xₙ] [y₁; …; yₘ]] returns [f (xₗ, yₗ) ∘ ⋯ ∘ f (x₁, y₁)]
    where [l = min n m].  The [t] suffix indicates truncation. *)


(** {2 Differential Iteration} *)

val map_diff : ('a -> 'a -> 'b) -> 'a list -> 'b list
(** [map_diff f [x₁; …; xₙ]] returns [f x₁ x₂; f x₂ x₃; …; f xₙ₋₁ xₙ].
    @raise Invalid_argument on the empty list. *)

val rev_map_diff : ('a -> 'a -> 'b) -> 'a list -> 'b list
(** [rev_map_diff f] is an optimisation of [rev ∘ map_diff f]. *)


(** {2 Sublists} *)

val drop : int -> 'a list -> 'a list
(** [drop n xs] returns the suffix of [xs] following the [n] first elements.
    Raises [Failure] if [xs] is shorter than [n] elements and Invalid_argument
    if [n] is negative. *)

val take : int -> 'a list -> 'a list
(** [take n xs] returns the [n]-element prefix of [xs].  Raises [Failure] if
    [xs] is shorter than [n] elements and Invalid_argument if [n] is
    negative. *)

val drop_while : ('a -> bool) -> 'a list -> 'a list
(** [drop_while f xs] returns the longest suffix of [xs] which does not start
    with an element on which [f] is true. *)

val take_while : ('a -> bool) -> 'a list -> 'a list
(** [take_while f xs] returns the longest prefix of [xs] containing only
    elements which [f] maps to true. *)
