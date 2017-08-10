(* Copyright (C) 2017  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Optional Values with Type-Level Information about Presence *)

type (+_, _) t =
  | Unknown : ('a, [> `Unknown]) t
  | Known : 'a -> ('a, [> `Known]) t (**)
(** [('a, 'k) t] is a type-enriched analogue of ['a option], where ['k]
    determines whether the value is present or not. *)

val get : ('a, [`Known]) t -> 'a
(** [know k] is the value held by the type-assured knowledge of it. *)

val get_opt : ('a, [< `Known | `Unknown]) t -> 'a option
(** [get_opt k] is [Option.map know (inquire k)]. *)

val of_option : 'a option -> ('a, [> `Known | `Unknown]) t
(** [of_option None] is [Unknown] and [knowable_of_option (Some x)] is
    [Known x]. *)

val inquire : ('a, [< `Known | `Unknown]) t -> ('a, [> `Known]) t option
(** [inquire k] checks if [k] is known, and if so adds assures this fact in the
    returned copy. *)
