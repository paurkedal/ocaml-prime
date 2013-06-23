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

(** Combinators and other Primitives *)

(** {2 Combinators} *)

val ident : 'a -> 'a
(** The I combinator: [ident x] is [x]. *)

val konst : 'a -> 'b -> 'a
(** The K combinator: [konst x y] is [x]. *)

val ( *< ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** The composition operator: [(f *< g) x] is [f (g x)]. *)

val ( *> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** The reversed composition operator: [(f *> g) x] is [g (f x)]. *)

val ( |> ) : 'a -> ('a -> 'b) -> 'b
(** The reversed application operator:
    [x |> f₁ |> ⋯ |> fₙ] is [fₙ (fₙ₋₁ ⋯ (f₁ x)⋯)]. *)

(** {2 Currying} *)

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
(** [curry f x y] is [f (x, y)]. *)

val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
(** [uncurry f (x, y)] is [f x y]. *)

(** {2 Exceptions} *)

val finally : (unit -> unit) -> (unit -> 'a) -> 'a
