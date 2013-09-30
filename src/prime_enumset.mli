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

(** Enumerating Set (testing). *)

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type elt
  type t

  val empty : t
  val singleton : elt -> t
  val contains : elt -> t -> bool
  val locate : elt -> t -> int option
  val get : int -> t -> elt
  val min_elt : t -> elt
  val max_elt : t -> elt
  val add : elt -> t -> t
  val remove : elt -> t -> t
  val pop_min : t -> elt * t
  val pop_max : t -> elt * t
  val card : t -> int
  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
  val iter : (elt -> unit) -> t -> unit
end

module Make (Elt : OrderedType) : S with type elt = Elt.t
