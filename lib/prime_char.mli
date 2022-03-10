(* Copyright (C) 2013--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

(** Amendment to the standard library [Char] structure.

    This structure adds a few functions to work directly on the single-byte
    encodings of ASCII code points.  All character class predicates returns
    false on bytes which are parts of non-ASCII characters.  For general
    processing of natural language texts, use a Unicode library.

    The original [Char] structure is not included here, see {!Unprime_char} for
    a full replacement. *)

val is_ascii : char -> bool
(** Maps a character to true iff its code is in the interval \[0, 127\]. *)

val is_ascii_graph : char -> bool
(** True on characters normally rendered as non-blank glyphs; that is all
    non-space printable characters. *)

val is_ascii_cntrl : char -> bool
(** True for control characters in the ASCII range. *)

val is_ascii_space : char -> bool
(** Maps space, tab, NL, CR, VT, FF to true, everything else to false. *)

val is_ascii_digit : char -> bool
(** True on latin digits ['0'] to ['9'], false elsewhere. *)

val is_xdigit : char -> bool
(** True on hexadecimal digits, false elsewhere. *)

val is_ascii_lower : char -> bool
(** True on ASCII lower-case letters ['a'] to ['z'], false elsewhere. *)

val is_ascii_upper : char -> bool
(** True on ASCII upper-case letters ['A'] to ['Z'], false elsewhere. *)

val is_ascii_alpha : char -> bool
(** True on ASCII letters, false elsewhere. *)

val is_ascii_alnum : char -> bool
(** True on ASCII letters and digits, false elsewhere. *)

val is_ascii_punct : char -> bool
(** True on non-alphanumeric ASCII graphic characters, false elsewhere. *)

val is_graph : char -> bool [@@deprecated "Renamed to is_ascii_graph."]
val is_space : char -> bool [@@deprecated "Renamed to is_ascii_space."]
val is_digit : char -> bool [@@deprecated "Renamed to is_ascii_digit."]
val is_lower : char -> bool [@@deprecated "Renamed to is_ascii_lower."]
val is_upper : char -> bool [@@deprecated "Renamed to is_ascii_upper."]
val is_alpha : char -> bool [@@deprecated "Renamed to is_ascii_alpha."]
val is_alnum : char -> bool [@@deprecated "Renamed to is_ascii_alnum."]
val is_punct : char -> bool [@@deprecated "Renamed to is_ascii_pnuct."]
