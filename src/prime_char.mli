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

(** Amendment to [Char].
    This structure adds a few functions to deal with ASCII characters.
    For processing text in natural languages, use a Unicode library.  The
    original [Char] structure is not included here, see {!Unprime_char} for a
    full replacement. *)

val is_ascii : char -> bool
(** Maps a character to true iff its code is in the interval \[0, 127\]. *)

val is_graph : char -> bool
(** True on characters normally rendered as non-blank glyphs; that is all
    non-space printable characters. *)

val is_space : char -> bool
(** Maps space, tab, NL, CR, VT, FF to true, everything else to false. *)

val is_digit : char -> bool
(** True on digits ['0'] to ['9'], false elsewhere. *)

val is_xdigit : char -> bool
(** True on hexadecimal digits, false elsewhere. *)

val is_lower : char -> bool
(** True on lower-case letters ['a'] to ['z'], false elsewhere. *)

val is_upper : char -> bool
(** True on upper-case letters ['A'] to ['Z'], false elsewhere. *)

val is_alpha : char -> bool
(** True on letters, false elsewhere. *)

val is_alnum : char -> bool
(** True on letters and digits, false elsewhere. *)

val is_punct : char -> bool
(** True on non-alphanumeric graphic characters, false elsewhere. *)
