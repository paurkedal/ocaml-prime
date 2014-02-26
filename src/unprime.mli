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

(** Pervasives. *)

include module type of Prime

(** {2 Option Operators} *)

val ( |?>. ) : 'a option -> ('a -> 'b) -> 'b option
(** An operator variant of {!Option.map}. *)

val ( |?>.. ) : 'a option -> ('a -> 'b -> 'b) -> 'b -> 'b
(** An operator variant of {!Option.fold}. *)

val ( |?>! ) : 'a option -> ('a -> unit) -> unit
(** An operator variant of {!Option.iter}. *)

val ( |?>= ) : 'a option -> ('a -> 'b option) -> 'b option
(** [None |?>= f] is [None] and [Some x |?>= f] is [f x].  This is the bind
    operator of the maybe monad. *)

(** {2 List Operators} *)

val ( |@>. ) : 'a list -> ('a -> 'b) -> 'b list
(** An operator variant of {!List.map}. *)

val ( |@>.. ) : 'a list -> ('a -> 'b -> 'b) -> 'b -> 'b
(** An operator variant of {!List.fold}. *)

val ( |@>! ) : 'a list -> ('a -> unit) -> unit
(** An operator variant of {!List.iter}. *)

val ( |@>= ) : 'a list -> ('a -> 'b list) -> 'b list
(** [[x₁; …; xₙ] |@>= f] computes [f x₁ @ ⋯ @ f xₙ].  This is the bind
    operator of the list monad. *)
