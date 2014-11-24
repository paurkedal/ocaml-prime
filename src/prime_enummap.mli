(* Copyright (C) 2013--2014  Petter Urkedal <paurkedal@gmail.com>
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

  val is_empty : 'e t -> bool
  (** Holds for the empty map only. *)

  val cardinal : 'a t -> int
  (** [cardinal m] is the cardinality of [m]. *)

  val contains : key -> 'a t -> bool
  (** [contains k m] is true iff [m] has a binding for [k]. *)

  val find : key -> 'a t -> 'a
  (** [find k m] returns the binding for [k] in [m] or raises [Not_found] if
      it is unbound. *)

  val locate : key -> 'a t -> bool * int
  (** [locate k m] is a pair [(present, pos)] where [present] is true iff [k]
      is bound in [m] and [pos] is the number of keys of [m] preceding [k]. *)

  val get_o : int -> 'a t -> 'a option
  (** [get_o i m] is the [i]th value of [m] or [None] if [i] is out of
      bounds. *)

  val get_e : int -> 'a t -> 'a
  (** [get_e i m] is the [i]th value of [m].
      @raise Invalid_argument if the index is out of bounds. *)

  val get_binding : int -> 'a t -> key * 'a
  (** [get_binding i m] is the [i]th binding of [m] according to the key
      order. *)

  val min_binding : 'a t -> key * 'a
  (** [min_binding m] is the binding of [m] with the smallest key. *)

  val max_binding : 'a t -> key * 'a
  (** [max_binding m] is the binding of [m] with the largest key. *)

  val pred_binding_e : 'a t -> key -> key * 'a

  val succ_binding_e : 'a t -> key -> key * 'a

  val pred_binding_o : 'a t -> key -> (key * 'a) option

  val succ_binding_o : 'a t -> key -> (key * 'a) option

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

  val cut : key -> 'a t -> 'a option * 'a t * 'a t

  val search : (key -> 'a -> 'b option) -> 'a t -> 'b option
  (** [search f m] is the first [f k e] for [k ↦ e] in [m] which is different
      from [None], or [None] if no such mapping exists in [m]. *)

  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold f m] is the composition of [f k e] for each [(k, e)] in [m],
      applied in order of increasing keys. *)

  val fold_rev : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold_rev f m] is the composition of [f k e] for each [(k, e)] in [m]
      applied in order of decreasing keys. *)

  val iter : (key -> 'a -> unit) -> 'a t -> unit
  (** [iter f m] calls [f k e] for each [(k, e)] in [m] in order of increasing
      keys. *)

  val for_all : (key -> 'a -> bool) -> 'a t -> bool
  (** [for_all f m] is true iff [f k e] is true for all bindings [(k, e)] of
      [m]. *)

  val exists : (key -> 'a -> bool) -> 'a t -> bool
  (** [exists f m] is true iff [f k e] for some binding [(k, e)] of [m]. *)

  val map : ('a -> 'b) -> 'a t -> 'b t
  (** [map f m] is the minimal map which for each [(k, e)] of [m] contains
      [(k, f e)]. *)

  val mapi : (key -> 'a -> 'b) -> 'a t -> 'b t
  (** [mapi f m] is the minimal map which for each [(k, e)] of [m] contains
      [(k, f k e)]. *)

  val fmapi : (key -> 'a -> 'b option) -> 'a t -> 'b t
  (** [fmapi f m] is the minimal map which contains [(k, e)] iff [f k e' =
      Some e] for some [(k, e')] in [m]. *)

  val filter : (key -> 'a -> bool) -> 'a t -> 'a t
  (** [filter f m] is the maximal submap of [m] such that [f k e] holds for
      each binding [(k, e)]. *)

  val compare : ('a -> 'b -> int) -> 'a t -> 'b t -> int
  (** [compare f] is a total order over maps using [f] to compare elements. *)

  val equal : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool
  (** [equal f m0 m1] is true iff [m0] and [m1] have the same keys and [f] is
      true on the respective mappings for each key. *)

  val merge : (key -> 'a option -> 'b option -> 'c option) ->
	      'a t -> 'b t -> 'c t

  val finter : (key -> 'a -> 'b -> 'c option) -> 'a t -> 'b t -> 'c t
  (** [finter f m1 m2] is the minimal map which for each shared key [k] of
      [m1] and [m2] contains [(k, e)] if [f k m1 m2 = Some e] for some [e]. *)

  val funion : (key -> 'a -> 'a -> 'a option) -> 'a t -> 'a t -> 'a t
  (** [funion f m1 m2] is the minimal map which contains all bindings of
      [finter f m1 m2] and all bindings of [m1] and [m2] whose key does not
      occur in the other map. *)

  val fcompl : (key -> 'a -> 'b -> 'b option) -> 'a t -> 'b t -> 'b t
  (** [fcompl f m1 m2] is the minimal map which contains the bindings of
      [finter f m1 m2] and the bindings of [m2] with the key not in [m1]. *)

  val fpatch : (key -> 'a -> 'b option -> 'b option) -> 'a t -> 'b t -> 'b t
  (** [fpatch f m1 m2] is the minimal map which contains the bindings of [m2]
      having keys disjoint from [m1], and which contains [(k, e)] iff [(k,
      e')] is in [m1] and [f k e' (find_o k m1) = Some e]. *)

  val split_union : (key -> 'a -> 'b -> 'c) ->
		    'a t -> 'b t -> 'a t * 'b t * 'c t
  (** [split_union mA mB] is a triple [(mA', mB', mC')] where [mA'] and [mB']
      are the respective bindings of [mA] and [mB] which have disjoint keys,
      and [mC'] has a binding [(k, f k a b)] for each pair of bindings [(k,
      a)] of [mA] and [(k, b)] of [mB] sharing [k]. *)

  val card : 'a t -> int
  (** @deprecated Use {!cardinal}. *)
end

module Make (Key : OrderedType) : S with type key = Key.t
