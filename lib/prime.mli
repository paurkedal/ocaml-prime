(* Copyright (C) 2013--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

(** {2 Optional Values with Type-Level Information about Presence} *)

type (+_, _) knowable =
  | Unknown : ('a, [> `Unknown]) knowable
  | Known : 'a -> ('a, [> `Known]) knowable (*/*)
(** [('a, 'k) knowable] is a type-enriched analogue of ['a option], where ['k]
    determines whether the value is present or not. *)

val know : ('a, [`Known]) knowable -> 'a
(** [know k] is the value held by the type-assured knowledge of it. *)

val inquire : ('a, [< `Known | `Unknown]) knowable ->
              ('a, [> `Known]) knowable option
(** [inquire k] checks if [k] is known, and if so adds assures this fact in the
    returned copy. *)

val option_of_knowable : ('a, [< `Known | `Unknown]) knowable -> 'a option
(** [option_of_knowable k] is [Option.map know (inquire k)]. *)

val knowable_of_option : 'a option -> ('a, [> `Known | `Unknown]) knowable
(** [knowable_of_option None] is [Unknown] and [knowable_of_option (Some x)] is
    [Known x]. *)

(** {2 Combinators} *)

val ident : 'a -> 'a
(** The I combinator: [ident x] is [x]. *)

val konst : 'a -> 'b -> 'a
(** The K combinator: [konst x y] is [x]. *)

val (<@) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
(** Function composition: [(g <@ f) x] is [g (f x)]. *)

val (@>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Reversed function composition: [(f @> g) x] is [g (f x)]. *)

(** {2 Currying} *)

val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
(** [curry f x y] is [f (x, y)]. *)

val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
(** [uncurry f (x, y)] is [f x y]. *)

(** {2 Exceptions} *)

val finally : (unit -> unit) -> (unit -> 'a) -> 'a

(**/**)
val ( *< ) : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c
[@@ocaml.deprecated "Renamed to (<@) for optimal associativity of (@>)."]
val ( *> ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
[@@ocaml.deprecated "Renamed to (@>) for optimal associativity."]
