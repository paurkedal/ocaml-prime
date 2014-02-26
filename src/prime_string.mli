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

(** Amendment to the standard library [String] structure.

    The original [String] structure is not included, include {!Unprime_string}
    for a full replacement. *)

(** {2 Construction and Conversion} *)

val sample : (int -> char) -> int -> string
val of_list : char list -> string
val to_list : string -> char list

(** {2 Iteration over Elements} *)

val fold : (char -> 'a -> 'a) -> string -> 'a -> 'a
val for_all : (char -> bool) -> string -> bool
val exists : (char -> bool) -> string -> bool
val filter : (char -> bool) -> string -> string
val search : (char -> 'a option) -> string -> 'a option

(** {2 Search Primitives} *)

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

val skip_affix : string -> string -> int -> int option
(** [skip_affix afx s i] returns the end position of the leftmost occurrence
    of [afx] in [s] which starts at or after [i].
    @raise Not_found if [afx] does not occur in [slice i (length s) s]. *)

val rskip_affix : string -> string -> int -> int option
(** [rskip_affix afx s j] retruns the start position of the rightmost
    occurrence of [afx] in [s] which ends before or at [j].
    @raise Not_found if [afx] does not occur in [slice 0 j s]. *)

(** {2 Substring Predicates} *)

val has_prefix : string -> string -> bool
(** [has_prefix pfx s] is true iff [s] starts with [pfx]. *)

val has_suffix : string -> string -> bool
(** [has_suffix sfx s] is true iff [s] ends with [sfx]. *)

val has_slice : int -> string -> string -> bool
(** [has_slice i ss s] is true iff [s] contains the substring [ss] starting at
    position [i]. *)

(** {2 Slicing} *)

val slice : int -> int -> string -> string
(** [slice i j s] is the slice of [s] from position [i] up to but not
    including [j]. *)

val slice_from : int -> string -> string
(** [slice_from i s] is a shortcut for [slice i (String.length s) s]. *)

val cut_affix : string -> string -> (string * string) option
(** [cut_affix afx s] returns the substrings before and after the leftmost
    occurrence of [afx] in [s].
    @raise Not_found if [afx] does not occur in [s]. *)

val rcut_affix : string -> string -> (string * string) option
(** [rcut_affix afx s] returns the substrings before and after the rightmost
    occurrence of [afx] in [s].
    @raise Not_found if [afx] does not occur in [s]. *)

val chop_affix : string -> string -> string list
(** [chop_affix afx s] returns the substrings before, between, and after
    matches of [afx] in [s], except for [chop_affix afx ""], which always
    gives [[]].  In other words [chop_affix afx] provides a primitive way of
    extracting the operands of an infix operator [afx].  If [afx] can overlap,
    it is unspecified which match is used. *)

val cut_consecutive : (char -> bool) -> string -> (string * string) option
(** [cut_consecutive f s] returns the substrings before and after the leftmost
    consecutive sequence of bytes satisfying [f], or [None] if not
    [String.exists f s]. *)

val rcut_consecutive : (char -> bool) -> string -> (string * string) option
(** [cut_consecutive f s] returns the substrings before and after the
    rightmost consecutive sequence of bytes satisfying [f], or [None] if not
    [String.exists f s]. *)

val chop_consecutive : (char -> bool) -> string -> string list
(** [chop_consecutive f s] returns the non-empty substrings before, between,
    and after consecutive sequences of bytes [c] for which [f c] is true.  In
    particular [chop_consecutive Char.is_space] is suitable for splitting
    words separated by ASCII white-spaces. *)
