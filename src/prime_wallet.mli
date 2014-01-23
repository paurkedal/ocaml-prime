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

type 'a t

val empty : 'a t
val is_empty : 'a t -> bool
val singleton : 'a -> 'a t
val sample : (int -> 'a) -> int -> 'a t
val length : 'a t -> int
val push : 'a -> 'a t -> 'a t
val pop : 'a t -> 'a * 'a t
val get : int -> 'a t -> 'a
val set : int -> 'a -> 'a t -> 'a t
val modify : int -> ('a -> 'a) -> 'a t -> 'a t
val map : ('a -> 'b) -> 'a t -> 'b t
val iter : ('a -> unit) -> 'a t -> unit
val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
