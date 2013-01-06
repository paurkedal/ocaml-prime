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

open Char

let is_ascii ch = ch <= '\x7f'
let is_graph ch = '!' <= ch && ch <= '~'
let is_space = function
  | ' ' | '\t' | '\n' | '\r' | '\x0b' | '\x0c' -> true
  | _ -> false

let is_digit ch = '0' <= ch && ch <= '9'
let is_xdigit ch =
  is_digit ch || ('a' <= ch && ch <= 'f' || 'A' <= ch && ch <= 'F')
let is_lower ch = 'a' <= ch && ch <= 'z'
let is_upper ch = 'A' <= ch && ch <= 'Z'
let is_alpha ch = is_lower ch || is_upper ch
let is_alnum ch = is_digit ch || is_alpha ch
let is_punct ch = is_graph ch && not (is_alnum ch)
