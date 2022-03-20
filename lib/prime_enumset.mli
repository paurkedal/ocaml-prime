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

(** Enumerating set ({e prime.unstable}).

    This is similar to the [Set] structure from the standard library, but
    provides a {!locate} method to tell the position of an element in the
    map. *)

open Prime_sigs

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

  val mem : elt -> t -> bool
  (** [mem e s] is true iff [s] contains [e]. *)

  val locate : elt -> t -> bool * int
  (** [locate e s] is a pair [(present, pos)] where [present] is true iff [e]
      is a member of [s], and [pos] is the number of elements of [s] which
      precedes [e]. *)

  val get : t -> int -> elt
  (** [get s i] is the [i]th element of [s] according to the element order. *)

  val choose : t -> elt
  (** [choose s] is an unspecified element of [s].
      @raise Not_found if [s] is empty. *)

  val min_elt : t -> elt option
  (** [min_elt s] is the smallest element of [s], or [None] if [s] is empty. *)

  val min_elt_exn : t -> elt
  (** [min_elt_exn s] is the smallest element of [s].
      @raise Not_found if the set is empty. *)

  val max_elt : t -> elt option
  (** [max_elt s] is the greatest element of [s], or [None] if [s] is empty. *)

  val max_elt_exn : t -> elt
  (** [max_elt_exn s] is the largest element of [s].
      @raise Not_found if the set is empty. *)

  val pred_elt : t -> elt -> elt option
  (** [pred_elt s e] is the element before [e] in [s]. *)

  val succ_elt : t -> elt -> elt option
  (** [succ_elt s e] is the element after [e] in [s]. *)

  val add : elt -> t -> t
  (** [add e s] is the set containing [e] along with the elements of [s]. *)

  val remove : elt -> t -> t
  (** [remove e s] is the set containing all elements of [s] except [e]. *)

  val cut_element : elt -> t -> bool * t * t
  (** [cut_element k s] is [(contains k s, sL, sR)] where [sL] and [sR] are
      the subsets of [s] with keys smaller and larger than [k],
      respectively. *)

  val pop_min : t -> elt * t [@@deprecated "Renamed to pop_min_exn."]
  (** @deprecated Renamed to pop_min_exn. *)

  val pop_min_exn : t -> elt * t
  (** [pop_min_exn s] is [(e, s')] where [e] is the smallest element of [s] and
      [s'] contains the remaining elements.
      @raise Not_found if the set is empty. *)

  val pop_max : t -> elt * t [@@deprecated "Renamed to pop_max_exn."]
  (** @deprecated Renamed to pop_max_exn. *)

  val pop_max_exn : t -> elt * t
  (** [pop_max_exn s] is [(e, s')] where [e] is the largest element of [s] and
      [s'] contains the remaining elements.
      @raise Not_found if the set is empty. *)

  val elements : t -> elt list
  (** [elements s] is the list of elements of [s] in order. *)

  val of_ordered_elements : elt list -> t
  (** [of_ordered_elements es] is the set containing precisely the elements
      [es], which must be listed in order.
      @raise Invalid_argument if [es] is not sorted. *)

  val asc_elements : ?where: (elt -> int) -> t -> elt Seq.t
  (** [asc_elements ?where s] is the sequence of elements of [e] in ascending
      order, optionally restricted to a subrange given by [where].

      @param where
        must be a monotonically increasing function, in which case the returned
        elements will be the continuous range at which it evaluates to zero. *)

  val dsc_elements : ?where: (elt -> int) -> t -> elt Seq.t
  (** [dsc_elements ?where s] is the sequence of elements of [e] in descending
      order, optionally restricted to a subrange given by [where].

      @param where
        must be a monotonically increasing function, in which case the returned
        elements will be the continuous range at which it evaluates to zero. *)

  val find_map : (elt -> 'a option) -> t -> 'a option
  (** [find_map f s] is the first [f e] for [e] in [s] which is different from
      [None], or [None] if there is no such [e]. *)

  val search : (elt -> 'a option) -> t -> 'a option
  [@@deprecated "Renamed to find_map."]

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f s] is the composition of [f e] for each [e] in [s] applied in
      order of increasing [e]. *)

  val fold_rev : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold_rev f s] is the composition of [f e] for [e] in [s] applied in
      order of decreasing [e]. *)

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

  val disjoint : t -> t -> bool
  (** [disjoint s0 s1] is true iff no element is contained in both [s0] and
      [s1]. *)

  val equal : t -> t -> bool
  (** [equal s0 s1] is true iff [s0] and [s1] contain the same elements. *)

  val subset : t -> t -> bool
  (** [subset s0 s1] is true iff [s0] is a subset or equal to [s1]. *)

  val union : t -> t -> t
  (** [union s1 s2] is the union of [s1] and [s2]. *)

  val inter : t -> t -> t
  (** [inter s1 s2] is the intersection of [s1] and [s2]. *)

  val compl : t -> t -> t
  (** [compl s1 s2] is the complement of [s1] relative to [s2]. *)

end

module type S_monadic = sig
  type elt
  type t
  type 'a monad
  val fold_s : (elt -> 'a -> 'a monad) -> t -> 'a -> 'a monad
  val iter_s : (elt -> unit monad) -> t -> unit monad
  val find_map_s : (elt -> 'a option monad) -> t -> 'a option monad
  val search_s : (elt -> 'a option monad) -> t -> 'a option monad
  [@@deprecated "Renamed to find_map_s."]
  val for_all_s : (elt -> bool monad) -> t -> bool monad
  val exists_s : (elt -> bool monad) -> t -> bool monad
  val filter_s : (elt -> bool monad) -> t -> t monad
end

module type S_with_monadic = sig
  include S
  include S_monadic with type elt := elt and type t := t
end

module Make (Elt : OrderedType) : sig
  include S with type elt = Elt.t

  module Make_monadic (Monad : Monad) :
    S_monadic with type elt := elt and type t := t
               and type 'a monad = 'a Monad.t
end

module Make_monadic (Elt : OrderedType) (Monad : Monad) :
  S_with_monadic with type elt = Elt.t and type 'a monad = 'a Monad.t
