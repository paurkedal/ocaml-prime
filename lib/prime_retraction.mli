(* Copyright (C) 2014--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

(** An ordered set of elements addressable by a property
    ({i prime.unstable}). *)

module type RETRACTABLE = sig
  type key
  type t
  val compare : t -> t -> int
  val compare_key : key -> t -> int
end

module type S = sig

  type key
  type elt
  type t

  val empty : t

  val singleton : elt -> t

  val is_empty : t -> bool

  val cardinal : t -> int

  val elements : t -> elt list

  (** {2 Element Inspection} *)

  val mem : key -> t -> bool

  val mem_elt : elt -> t -> bool

  val app : t -> key -> elt option

  val find : key -> t -> elt

  val locate : key -> t -> bool * int

  val locate_elt : elt -> t -> bool * int

  val get : t -> int -> elt

  val min_exn : t -> elt

  val max_exn : t -> elt

  val pred_exn : t -> key -> elt

  val succ_exn : t -> key -> elt

  val elt_pred_exn : t -> elt -> elt

  val elt_succ_exn : t -> elt -> elt

  (** {2 Element Updates} *)

  val add : elt -> t -> t

  val pop : key -> t -> (elt * t) option

  val pop_min_exn : t -> elt * t

  val pop_max_exn : t -> elt * t

  val remove : key -> t -> t

  val cut : key -> t -> elt option * t * t

  (** {2 Iteration} *)

  val search : (elt -> 'a option) -> t -> 'a option

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val fold_rev : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val iter : (elt -> unit) -> t -> unit

  val for_all : (elt -> bool) -> t -> bool

  val exists : (elt -> bool) -> t -> bool

  val filter : (elt -> bool) -> t -> t

  (** {2 Algebra} *)

  val compare : t -> t -> int

  val equal : t -> t -> bool

  val finter : (elt -> elt -> elt option) -> t -> t -> t

  val funion : (elt -> elt -> elt option) -> t -> t -> t

  val fcompl : (elt -> elt -> elt option) -> t -> t -> t

end

module Make (Elt : RETRACTABLE) : S with type elt = Elt.t and type key = Elt.key
