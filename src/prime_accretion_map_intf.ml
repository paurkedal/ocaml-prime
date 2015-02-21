(* Copyright (C) 2015  Petter Urkedal <paurkedal@gmail.com>
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

(** Accumulation of a monoid over a map {i prime.unstable}. *)

module type Monoid1 = sig
  type 'a t

  val empty : 'a t

  val cat : 'a t -> 'a t -> 'a t
end

module type Monoid = sig
  type t

  val empty : t

  val cat : t -> t -> t
end

module type S1 = sig
  type key
  type 'a elt
  type 'a t

  val empty : 'a t

  val singleton : key -> 'a elt -> 'a t

  val is_empty : 'a t -> bool

  val cardinal : 'a t -> int

  val result : 'a t -> 'a elt

  val add : key -> 'a elt -> 'a t -> 'a t

  val remove : key -> 'a t -> 'a t

  val bindings : 'a t -> (key * 'a elt) list
end

module type S = sig
  type key
  type elt
  type t

  val empty : t

  val singleton : key -> elt -> t

  val is_empty : t -> bool

  val cardinal : t -> int

  val result : t -> elt

  val add : key -> elt -> t -> t

  val remove : key -> t -> t

  val bindings : t -> (key * elt) list
end
