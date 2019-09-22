(* Copyright (C) 2013--2019  Petter A. Urkedal <paurkedal@gmail.com>
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

let is_ascii ch = ch <= '\x7f'
let is_ascii_graph ch = '!' <= ch && ch <= '~'
let is_ascii_space = function
  | ' ' | '\t' | '\n' | '\r' | '\x0b' | '\x0c' -> true
  | _ -> false

let is_ascii_digit ch = '0' <= ch && ch <= '9'
let is_xdigit ch =
  is_ascii_digit ch || ('a' <= ch && ch <= 'f' || 'A' <= ch && ch <= 'F')
let is_ascii_lower ch = 'a' <= ch && ch <= 'z'
let is_ascii_upper ch = 'A' <= ch && ch <= 'Z'
let is_ascii_alpha ch = is_ascii_lower ch || is_ascii_upper ch
let is_ascii_alnum ch = is_ascii_digit ch || is_ascii_alpha ch
let is_ascii_punct ch = is_ascii_graph ch && not (is_ascii_alnum ch)

let is_graph = is_ascii_graph
let is_space = is_ascii_space
let is_digit = is_ascii_digit
let is_lower = is_ascii_lower
let is_upper = is_ascii_upper
let is_alpha = is_ascii_alpha
let is_alnum = is_ascii_alnum
let is_punct = is_ascii_punct
