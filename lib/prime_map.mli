(* Copyright (C) 2013--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Amendment to the standard library [Map] structure. *)

module type OrderedType = Map.OrderedType

module type S = sig
  include Map.S

  val app : 'a t -> key -> 'a option
  (** [app m] is the partial function corresponding to [m]. *)

  val pop : key -> 'a t -> ('a * 'a t) option
  (** [pop k m] returns [Some (find k m, remove k m)] if [k] is bound in [m],
      otherwise [None]. *)

  val find_map : (key -> 'a -> 'b option) -> 'a t -> 'b option
  (** [find_map f m] returns the first non-[None] result of [f k v] where [k, v]
      runs over the bindings of [m] if it exists, otherwise [None]. *)

  val fold_inter : (key -> 'a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
  (** [fold_inter f m0 m1] returns the composition [f kₙ vₙ wₙ ∘ ⋯ ∘ f k₁ v₁ w₁]
      where [k₁, …, kₙ] are the coinciding indices of [m0] and [m1], and [v₁,
      …, vₙ] and [w₁, …, wₙ] are the correspoding values from [m0] and [m1],
      respectively. *)

  val inter : (key -> 'a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
  (** [inter f m1 m2] is a new map whose keys are the intersection of the keys
      of [m1] and [m2] filtered by the optionality of the result, and each
      mapping to the corresponding non-[None] results.
      This is the special case of {!Stdlib.Map.merge}, restricted to the
      intersecting keys. *)

  val left_union : 'a t -> 'a t -> 'a t
  (** [left_union m0 m1] is the map whose domain is the union of the domains
      of [m0] and [m1] and whose values agree with [m0] where present and [m1]
      elsewhere. *)

  val split_union : (key -> 'a -> 'b -> 'c) ->
                    'a t -> 'b t -> 'a t * 'b t * 'c t
  (** [split_union mA mB] is a triple [(mA', mB', mC')] where [mA'] and [mB']
      are the respective bindings of [mA] and [mB] which have disjoint keys,
      and [mC'] has a binding [(k, f k a b)] for each pair of bindings [(k,
      a)] of [mA] and [(k, b)] of [mB] sharing [k]. *)

  val left_inter : 'a t -> 'b t -> 'a t
  (** [left_inter m1 m2] is [inter (fun _ x _ -> Some x) m1 m2]. *)

  val compl : 'a t -> 'a t -> 'a t
  (** [compl mN mP] is the complement of [mN] relative to [mP]. *)

  val search : (key -> 'a -> 'b option) -> 'a t -> 'b option
  [@@deprecated "Renamed to find_map."]
  val fold2t : (key -> 'a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c
  [@@deprecated "Renamed to fold_inter."]
  val map2t : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  [@@deprecated "Use inter."]
  val mapi2t : (key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
  [@@deprecated "Use inter."]
end

module Make (K : OrderedType) : S with type key = K.t
