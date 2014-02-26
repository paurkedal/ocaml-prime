(* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 2 of the License, or (at your
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

(** Enumerating set ({e prime.testing}).
    
    This is similar to the [Set] structure from the standard library, but
    provides a {!locate} method to tell the position of an element in the
    map. *)

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig

  type elt
  (** The type of the elements of the set. *)

  type t
  (** The type of the set. *)

  val empty : t
  (** The empty set. *)

  val singleton : elt -> t
  (** [singleton e] is the set containing only [e]. *)

  val contains : elt -> t -> bool
  (** [contains e s] is true iff [s] contains [e]. *)

  val locate : elt -> t -> int option
  (** If [s] contains [e], then [locate e s] is [Some i] where [i] is the
      number of elements in [s] which are smaller than [e], otherwise [locate
      e s] is [None]. *)

  val get : int -> t -> elt
  (** [get i s] is the [i]th element of [s] according to the element order. *)

  val min_elt : t -> elt
  (** [min_elt s] is the smallest element of [s]. *)

  val max_elt : t -> elt
  (** [max_elt s] is the largest element of [s]. *)

  val add : elt -> t -> t
  (** [add e s] is the set containing [e] along with the elements of [s]. *)

  val remove : elt -> t -> t
  (** [remove e s] is the set containing all elements of [s] except [e]. *)

  val pop_min : t -> elt * t
  (** [pop_min s] is [(e, s')] where [e] is the smallest element of [s] and
      [s'] contains the remaining elements. *)

  val pop_max : t -> elt * t
  (** [pop_max s] is [(e, s')] where [e] is the largest element of [s] and
      [s'] contains the remaining elements. *)

  val card : t -> int
  (** [card s] is the cardinality of [s]. *)

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f s] is the composition of [f e] for each [e] in [s] applied in
      order of increasing [e]. *)

  val iter : (elt -> unit) -> t -> unit
  (** [iter f s] calls [f e] for each [e] in [s] in order of increasing
      elements. *)
end

module Make (Elt : OrderedType) : S with type elt = Elt.t
