(* Copyright (C) 2015--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Accumulation of a monoid over a map ({e prime.unstable}). *)

(** A monoid on a polymorphic type. *)
module type Monoid1 = sig
  type 'a t

  val empty : 'a t
  (** The identity element of {!cat}. *)

  val cat : 'a t -> 'a t -> 'a t
  (** The binary operator, which must be associative. *)
end

(** A monoid structure. *)
module type Monoid = sig
  type t

  val empty : t
  (** The identity element of {!cat}. *)

  val cat : t -> t -> t
  (** The binary operator, which must be associative. *)
end

(** A monoid on a polymorphic type with an explicit generator type. *)
module type MonoidG1 = sig
  include Monoid1

  type 'a generator

  val of_generator : 'a generator -> 'a t
end

(** A monoid with an explicit generator type. *)
module type MonoidG = sig
  include Monoid

  type generator

  val of_generator : generator -> t
end

(** Signature for an accretion map with polymorphic elements. *)
module type S1 = sig

  type key
  type 'a elt
  type 'a result
  type 'a t

  val empty : 'a t
  (** The empty map. *)

  val singleton : key -> 'a elt -> 'a t
  (** [singleton k e] is the map containing just [e] bound to [k]. *)

  val is_empty : 'a t -> bool
  (** [is_empty m] is true iff [m] is the empty map. *)

  val cardinal : 'a t -> int
  (** [cardinal m] is the number of bindings in [m]. *)

  val result : 'a t -> 'a result
  (** [result m] is the nested application of the monoid operator over
      elements of the map in key-order but arbitrary associative combination. *)

  val app : 'a t -> key -> 'a elt option
  (** [app m] is the partial function corresponding to the bindings of [m]. *)

  val find : key -> 'a t -> 'a elt
  (** [find k m] is the element mapped to [k].
      @raise Not_found if [k] has no binding in [m] *)

  val add : key -> 'a elt -> 'a t -> 'a t
  (** [add k e m] is the least map binding [k] to [e] and any [k' ≠ k] to
      [find k' m]. *)

  val remove : key -> 'a t -> 'a t
  (** [remove k m] is the minimal map binding [k' ≠ k] to [find k' m]. *)

  val bindings : 'a t -> (key * 'a elt) list
  (** [bindings m] is the list containing [(k, e)] iff [k] is bound to [e] in
      [m]. *)

  val fold : (key -> 'a elt -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold f m] is [f kₙ eₙ ∘ ... ∘ f k₁ e₁] where [(kᵢ, eᵢ)] are the
      bindings of [m] in key order. *)

  val iter : (key -> 'a elt -> unit) -> 'a t -> unit
  (** [iter f m] calls [f k e] for each binding [(k, e)] of [m] in key order. *)

  val search : (key -> 'a elt -> 'b option) -> 'a t -> 'b option
  (** [search f m] is [f k e] where [(k, e)] is the binding with smallest [k]
      for which [f k e ≠ None], or [None] if [m] has no such binding. *)

  val for_all : (key -> 'a elt -> bool) -> 'a t -> bool
  (** [for_all f m] is true iff [f k e] is true for all bindings [(k, e)] of
      [m]. *)

  val exists : (key -> 'a elt -> bool) -> 'a t -> bool
  (** [exists f m] is true iff there is a bindng [(k, e)] of [m] such that [f
      k e]. *)
end

(** Signature for an accretion map with monomorphic elements.
    See {!S1} for documentation. *)
module type S = sig
  type key
  type elt
  type result
  type t

  val empty : t

  val singleton : key -> elt -> t

  val is_empty : t -> bool

  val cardinal : t -> int

  val result : t -> result

  val app : t -> key -> elt option

  val find : key -> t -> elt

  val add : key -> elt -> t -> t

  val remove : key -> t -> t

  val bindings : t -> (key * elt) list

  val fold : (key -> elt -> 'a -> 'a) -> t -> 'a -> 'a

  val iter : (key -> elt -> unit) -> t -> unit

  val search : (key -> elt -> 'a option) -> t -> 'a option

  val for_all : (key -> elt -> bool) -> t -> bool

  val exists : (key -> elt -> bool) -> t -> bool
end
