(* Copyright (C) 2013--2014  Petter Urkedal <paurkedal@gmail.com>
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

(** Enumerating set ({e prime.unstable}).
    
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

  val is_empty : t -> bool
  (** Holds for the empty set only. *)

  val cardinal : t -> int
  (** [cardinal s] is the cardinality of [s]. *)

  val contains : elt -> t -> bool
  (** [contains e s] is true iff [s] contains [e]. *)

  val locate : elt -> t -> bool * int
  (** [locate e s] is a pair [(present, pos)] where [present] is true iff [e]
      is a member of [s], and [pos] is the number of elements of [s] which
      precedes [e]. *)

  val get : int -> t -> elt
  (** [get i s] is the [i]th element of [s] according to the element order. *)

  val min_elt : t -> elt
  (** [min_elt s] is the smallest element of [s]. *)

  val max_elt : t -> elt
  (** [max_elt s] is the largest element of [s]. *)

  val pred_e : t -> elt -> elt

  val succ_e : t -> elt -> elt

  val add : elt -> t -> t
  (** [add e s] is the set containing [e] along with the elements of [s]. *)

  val remove : elt -> t -> t
  (** [remove e s] is the set containing all elements of [s] except [e]. *)

  val cut : elt -> t -> bool * t * t

  val pop_min : t -> elt * t
  (** [pop_min s] is [(e, s')] where [e] is the smallest element of [s] and
      [s'] contains the remaining elements. *)

  val pop_max : t -> elt * t
  (** [pop_max s] is [(e, s')] where [e] is the largest element of [s] and
      [s'] contains the remaining elements. *)

  val search : (elt -> 'a option) -> t -> 'a option
  (** [search f s] is the first [f e] for [e] in [s] which is different from
      [None], or [None] if there is no such [e]. *)

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f s] is the composition of [f e] for each [e] in [s] applied in
      order of increasing [e]. *)

  val iter : (elt -> unit) -> t -> unit
  (** [iter f s] calls [f e] for each [e] in [s] in order of increasing
      elements. *)

  val for_all : (elt -> bool) -> t -> bool
  (** [for_all f s] holds if [f e] holds for all [e] in [s]. *)

  val exists : (elt -> bool) -> t -> bool
  (** [exists f s] holds if [f e] holds for some [e] in [s]. *)

  val filter : (elt -> bool) -> t -> t
  (** [filter f s] is the maximal subset of [s] such that [f e] holds all its
      elements. *)

  val compare : t -> t -> int
  (** Total order. *)

  val equal : t -> t -> bool
  (** [equal s0 s1] is true iff [s0] and [s1] contain the same elements. *)

  val union : t -> t -> t
  (** [union s1 s2] is the union of [s1] and [s2]. *)

  val inter : t -> t -> t
  (** [inter s1 s2] is the intersection of [s1] and [s2]. *)

  val compl : t -> t -> t
  (** [compl s1 s2] is the complement of [s1] relative to [s2]. *)

  val card : t -> int
  (** @deprecated Use {!cardinal}. *)
end

module Make (Elt : OrderedType) : S with type elt = Elt.t
