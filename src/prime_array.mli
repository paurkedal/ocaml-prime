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

(** Amendment to [Array]. *)

val sample : (int -> 'a) -> int -> 'a array
(** [sample f n] returns [[|f 0; …; f (n - 1)|]].  This calls [init] with
    arguments reversed. *)

val fold : ('a -> 'b -> 'b) -> 'a array -> 'b -> 'b
(** [fold f [|x₁; …; xₙ|]] is the composition [f xₙ ∘ ⋯ ∘ f x₁].  This is
    [fold_left] with arguments reorderd to make it easy to nest and compose. *)

val foldi : (int -> 'a -> 'b -> 'b) -> 'a array -> 'b -> 'b
(** [foldi f [|x₁; …; xₙ|]] returns the composition
    [f (n - 1) xₙ₋₁ ∘ ⋯ ∘ f 0 x₀]. *)

val for_all : ('a -> bool) -> 'a array -> bool
(** [for_all f [|x₁; …; xₙ|]] is true iff [f xᵢ] is true for all [i].  [f] is
    applied to the elements in order until false is returned. *)

val exists : ('a -> bool) -> 'a array -> bool
(** [exists f [|x₁; …; xₙ|]] is true iff [f xᵢ] is true for some [i].  [f] is
    applied to the elements in order until true is returned. *)

val search : ('a -> 'b option) -> 'a array -> 'b option
(** [search f xa] returns the first element of [map f xa] which is not [None],
    or [None] if there is no such element. *)

val slice : int -> int -> 'a array -> 'a array
(** [slice i j xa] returns the subarray from index [i] inclusive to index [j]
    exclusive. *)
