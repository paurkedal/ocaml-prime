(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

(** An ordered set of elements addressable by a property. *)

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

  val cardinal : t -> int

  val contains : key -> t -> bool

  val find_e : key -> t -> elt

  val find_o : key -> t -> elt option

  val locate_e : key -> t -> int

  val locate_o : key -> t -> int option

  val locate_elt_e : elt -> t -> int

  val locate_elt_o : elt -> t -> int option

  val get_e : int -> t -> elt

  val get_o : int -> t -> elt option

  val min_e : t -> elt

  val max_e : t -> elt

  val add : elt -> t -> t

  val pop_min_e : t -> elt * t

  val pop_max_e : t -> elt * t

  val remove : key -> t -> t

  val search : (elt -> 'a option) -> t -> 'a option

  val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a

  val iter : (elt -> unit) -> t -> unit

end

module Make (Elt : RETRACTABLE) : S with type elt = Elt.t and type key = Elt.key