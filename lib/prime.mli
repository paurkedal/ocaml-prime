(* Copyright (C) 2013--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Primitives. *)

(** {2 The Empty Type} *)

type counit
(** A type which is uninhabited by well-founded code.  This is equivalent to a
    variant type with no constructors, though syntax forbids naming or
    defining such types. *)

val absurd : counit -> 'a
(** Computations in the scope of a variable [x : counit] can be assumed dead,
    and thus be shortcut as [absurd x].  This is the analogue of
    pattern-matching a variant with no constructors. *)

(** {2 Combinators} *)

val ident : 'a -> 'a
(** The I combinator: [ident x] is [x]. *)

val konst : 'a -> 'b -> 'a
(** The K combinator: [konst x y] is [x]. *)

val (%) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** Function composition: [(g % f) x] is [g (f x)]. *)

val (%>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Reversed function composition: [(f %> g) x] is [g (f x)]. *)

(** {2 Currying} *)

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
(** [curry f x y] is [f (x, y)]. *)

val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
(** [uncurry f (x, y)] is [f x y]. *)

(** {2 Exceptions} *)

val finally : (unit -> unit) -> (unit -> 'a) -> 'a
