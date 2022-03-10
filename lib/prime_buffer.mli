(* Copyright (C) 2015--2022  Petter A. Urkedal <paurkedal@gmail.com>
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or (at your
 * option) any later version, with the LGPL-3.0 Linking Exception.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Lesser General Public
 * License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * and the LGPL-3.0 Linking Exception along with this library.  If not, see
 * <http://www.gnu.org/licenses/> and <https://spdx.org>, respectively.
 *)

(** Amendment to the standard library [Buffer] structure. *)

val with0 : ?n: int -> (Buffer.t -> unit) -> string
(** [with0 f] calls [f buf] on a fresh buffer [buf] and returns its content. *)

val with1 : ?n: int -> (Buffer.t -> 'a -> unit) -> 'a -> string
(** [with1 f x0] calls [f buf x0] on a fresh buffer [buf] and returns its
    content. *)

val with2 : ?n: int -> (Buffer.t -> 'a -> 'b -> unit) -> 'a -> 'b -> string
(** [with2 f x0 x1] calls [f buf x0 x1] on a fresh buffer [buf] and returns its
    content. *)
