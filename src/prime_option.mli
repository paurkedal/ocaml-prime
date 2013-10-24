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

(** Functions on ['a option]. *)

type 'a t = 'a option

val get : 'a option -> 'a
(** [get (Some a)] returns [a], and [get None] raises [Invalid_argument]. *)

val get_or : 'a -> 'a option -> 'a
(** [get_or d] is a variant of {!get} which maps [None] to [d] instead of
    raising an exception. *)

val get_else : (unit -> 'a) -> 'a option -> 'a
(** [get_else h] is a variant of {!get} which calls [h ()] to handle the
    [None] case instead of raising an exception. *)

val search : ('a -> 'b option) -> 'a option -> 'b option
(** [search f None] is [None] and [search f (Some a)] is [f a]. *)

val flatten : 'a option option -> 'a option
(** [flatten] maps [Some (Some x)] to [Some x] and other values to [None]. *)


(** {2 Iteration} *)

val iter : ('a -> unit) -> 'a option -> unit
(** [iter f (Some a)] calls [f a], and [iter f None] does nothing. *)

val map : ('a -> 'b) -> 'a option -> 'b option
(** [map f None] is [None] and [map f (Some a)] is [Some (f a)]. *)

val fold : ('a -> 'b -> 'b) -> 'a option -> 'b -> 'b
(** [fold f None] is the identity function and [fold f (Some a)] is [f a]. *)

val for_all : ('a -> bool) -> 'a option -> bool
(** [for_all f None] is true and [for_all f (Some a)] is [f a]. *)

val exists : ('a -> bool) -> 'a option -> bool
(** [exists f None] is false and [exists f (Some a)] is [f a]. *)

val filter : ('a -> bool) -> 'a option -> 'a option
(** [filter f (Some a)] returns [Some a] if [f a] is true; in other cases
    [filter f a] returns [None]. *)
