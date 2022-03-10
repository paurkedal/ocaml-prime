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

(** Amendment to the standard library [Array] structure. *)

val sample : (int -> 'a) -> int -> 'a array
(** [sample f n] returns [[|f 0; …; f (n - 1)|]].  This calls [init] with
    arguments reversed. *)

val filter : ('a -> bool) -> 'a array -> 'a array
(** [filter f xa] is the array containing the elements of [xa] on which [f] is
    true with order preserved. *)

val fmap : ('a -> 'b option) -> 'a array -> 'b array
[@@deprecated "Renamed back to filter_map for consistency with stdlib."]

val filter_map : ('a -> 'b option) -> 'a array -> 'b array
(** [filter_map f xa] is the array of the non-[None] elements of [map f xa],
    preserving order. *)

val fold : ('a -> 'b -> 'b) -> 'a array -> 'b -> 'b
(** [fold f [|x₁; …; xₙ|]] is [f xₙ ∘ ⋯ ∘ f x₁]. *)

val foldi : (int -> 'a -> 'b -> 'b) -> 'a array -> 'b -> 'b
(** [foldi f [|x₁; …; xₙ|]] is [f (n - 1) xₙ ∘ ⋯ ∘ f 0 x₁]. *)

val for_all : ('a -> bool) -> 'a array -> bool
(** [for_all f [|x₁; …; xₙ|]] is true iff [f xᵢ] is true for all [i].  [f] is
    applied to the elements in order until false is returned. *)

val exists : ('a -> bool) -> 'a array -> bool
(** [exists f [|x₁; …; xₙ|]] is true iff [f xᵢ] is true for some [i].  [f] is
    applied to the elements in order until true is returned. *)

val count : ('a -> bool) -> 'a array -> int
(** [count f xa] is the number of elements [x] of [xa] for which [f x]
    holds. *)

val search : ('a -> 'b option) -> 'a array -> 'b option
(** [search f xa] returns the first element of [map f xa] which is not [None],
    or [None] if there is no such element. *)

val slice : int -> int -> 'a array -> 'a array
(** [slice i j xa] returns the subarray from index [i] inclusive to index [j]
    exclusive. *)
