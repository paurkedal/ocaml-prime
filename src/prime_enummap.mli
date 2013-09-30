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

(** Enumerating Map (testing). *)

module type OrderedType = sig
  type t
  val compare : t -> t -> int
end

module type S = sig
  type key
  type 'a t

  val empty : 'a t
  val singleton : key -> 'a -> 'a t
  val contains : key -> 'a t -> bool
  val find : key -> 'a t -> 'a
  val locate : key -> 'a t -> int option
  val get_binding : int -> 'a t -> key * 'a
  val min_binding : 'a t -> key * 'a
  val max_binding : 'a t -> key * 'a
  val add : key -> 'a -> 'a t -> 'a t
  val pop_min : 'a t -> key * 'a * 'a t
  val pop_max : 'a t -> key * 'a * 'a t
  val remove : key -> 'a t -> 'a t
  val card : 'a t -> int
  val fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b
  val iter : (key -> 'a -> unit) -> 'a t -> unit
end

module Make (Key : OrderedType) : S with type key = Key.t
