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

let is_ascii = function '\x00'..'\x7f' -> true | _ -> false
let is_ascii_graph = function '!'..'~' -> true | _ -> false
let is_ascii_space = function
 | ' ' | '\t' | '\n' | '\r' | '\x0b' | '\x0c' -> true
 | _ -> false
let is_ascii_cntrl = function
 | '\x00'..'\x1f' | '\x7f' -> true
 | _ -> false

let is_ascii_digit = function '0'..'9' -> true | _ -> false
let is_xdigit = function '0'..'9' | 'a'..'f' | 'A'..'F' -> true | _ -> false
let is_ascii_lower = function 'a'..'z' -> true | _ -> false
let is_ascii_upper = function 'A'..'Z' -> true | _ -> false
let is_ascii_alpha = function 'a'..'z' | 'A'..'Z' -> true | _ -> false
let is_ascii_alnum = function '0'..'9' | 'a'..'z' | 'A'..'Z' -> true | _ ->false

let is_ascii_punct = function
 | '!'..'/' | ':'..'@' | '['..'`' | '{'..'~' -> true
 | _ -> false

let is_graph = is_ascii_graph
let is_space = is_ascii_space
let is_digit = is_ascii_digit
let is_lower = is_ascii_lower
let is_upper = is_ascii_upper
let is_alpha = is_ascii_alpha
let is_alnum = is_ascii_alnum
let is_punct = is_ascii_punct
