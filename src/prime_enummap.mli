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

(** Enumerated map ({e prime.unstable}).
    
    This is similar to [Map] from the standard library, but provides a
    {!locate} method which return the position of a binding within a map. *)

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig

  type key
  (** The type of the keys of the map. *)

  type 'a t
  (** The type of the map. *)

  val empty : 'a t
  (** The empty map. *)

  val singleton : key -> 'a -> 'a t
  (** [singleton k e] is the one-element map binding [k] to [e]. *)

  val contains : key -> 'a t -> bool
  (** [contains k m] is true iff [m] has a binding for [k]. *)

  val find : key -> 'a t -> 'a
  (** [find k m] returns the binding for [k] in [m] or raises [Not_found] if
      it is unbound. *)

  val locate : key -> 'a t -> int option
  (** If [k] is bound in [m], then [locate k m] is [Some i] where [i] is the
      number of other keys before [k] according to the key order, otherwise it
      returns [None]. *)

  val get_binding : int -> 'a t -> key * 'a
  (** [get_binding i m] is the [i]th binding of [m] according to the key
      order. *)

  val min_binding : 'a t -> key * 'a
  (** [min_binding m] is the binding of [m] with the smallest key. *)

  val max_binding : 'a t -> key * 'a
  (** [max_binding m] is the binding of [m] with the largest key. *)

  val add : key -> 'a -> 'a t -> 'a t
  (** [add k e m] is the map which agrees with [m] on all keys except that [k]
      is mapped to [e]. *)

  val pop_min : 'a t -> key * 'a * 'a t
  (** [pop_min m] is the tuple [(k, e, m')] where [(k, e)] is the binding of
      [m] with the minimal key and [m'] is the remainder of [m]. *)

  val pop_max : 'a t -> key * 'a * 'a t
  (** [pop_max m] is the tuple [(k, e, m')] where [(k, e)] is the binding of
      [m] with the maximum key and [m'] is the remainder of [m]. *)

  val remove : key -> 'a t -> 'a t
  (** [remove k m] is the map which agrees with [m] on all keys except that
      [k] is unbound.  If [k] is unbound in [m], then [remove k m] is [m]. *)

  val card : 'a t -> int
  (** [card m] is the cardinality of [m]. *)

  val search : (key -> 'a -> 'b option) -> 'a t -> 'b option
  (** [search f m] is the first [f k e] for [k ↦ e] in [m] which is different
      from [None], or [None] if no such mapping exists in [m]. *)

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold f m] is the composition of [f k e] for each [(k, e)] in [m],
      applied in order of increasing keys. *)

  val iter : (key -> 'a -> unit) -> 'a t -> unit
  (** [iter f m] calls [f k e] for each [(k, e)] in [m] in order of increasing
      keys. *)
end

module Make (Key : OrderedType) : S with type key = Key.t
