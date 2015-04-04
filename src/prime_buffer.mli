(* Copyright (C) 2015  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Amendment to the standard library [Buffer] structure. *)

val with0 : ?n: int -> (Buffer.t -> unit) -> string
(** [with0 f] executes [f buf] on a fresh buffer [buf] and returns its
    content. *)

val with1 : ?n: int -> (Buffer.t -> 'a -> unit) -> 'a -> string
(** [with1 f x0] executes [f buf x0] on a fresh buffer [buf] and returns its
    content. *)

val with2 : ?n: int -> (Buffer.t -> 'a -> 'b -> unit) -> 'a -> 'b -> string
(** [with2 f x0] executes [f buf x0 x1] on a fresh buffer [buf] and returns
    its content. *)

val with_buffer : ?n: int -> (Buffer.t -> unit) -> string
(** @deprecated Use {!with0}. *)
