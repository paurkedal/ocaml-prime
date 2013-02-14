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

(** Amendment to [String].
    The original [String] structure is not included, include {!Unprime_string}
    for a full replacement. *)

(** {6 Construction and Conversion} *)

val sample : (int -> char) -> int -> string
val of_list : char list -> string
val to_list : string -> char list

(** {6 Iteration} *)

val fold : (char -> 'a -> 'a) -> string -> 'a -> 'a
val for_all : (char -> bool) -> string -> bool
val exists : (char -> bool) -> string -> bool
val filter : (char -> bool) -> string -> string
val search : (char -> 'a option) -> string -> 'a option

(** {6 Search Primitives} *)

val skip_while : (char -> bool) -> string -> int -> int
(** [skip_while f s] increments a position until it reaches the end of [s] or
    it points to a character in [s] on which [f] returns false.  In other
    words if [j = skip_while f s i], then [slice i j s] is the largest
    substring starting at [i] on which [f] is constant true. *)

val skip_until : (char -> bool) -> string -> int -> int
(** [skip_until f] is an optimisation of [skip_while (not ∘ f)]. *)

val rskip_while : (char -> bool) -> string -> int -> int
(** [rskip_while f s] decrements a position as long as the result points to a
    character in [s] on which [f] is true. *)

val rskip_until : (char -> bool) -> string -> int -> int
(** [rskip_until f] is an optimization of [rskip_while (not ∘ f)]. *)

(** {6 Slices} *)

val slice : int -> int -> string -> string
(** [slice i j s] is the slice of [s] from position [i] up to but not
    including [j]. *)

val has_prefix : string -> string -> bool
(** [has_prefix pfx s] is true iff [s] starts with [pfx]. *)

val has_suffix : string -> string -> bool
(** [has_suffix sfx s] is true iff [s] ends with [sfx]. *)

val has_slice : int -> string -> string -> bool
(** [has_slice i ss s] is true iff [s] contains the substring [ss] starting at
    position [i]. *)

val find_slice : ?start: int -> string -> string -> int option
(** [find_slice ss s] locates the first occurrence of [ss] in [s], staring at
    [spos] which defaults to 0.  If found, returns [Some pos] where [pos] is
    the position of the first match character, else returns [None]. *)
