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

include Unprime_list
include Unprime_char
include Unprime_string

let run () =
  let s = "I won't make bugs.  I won't make bugs.  I won't make bugs." in
  let s' = "IwontmakebugsIwontmakebugsIwontmakebugs" in
  let n = String.length s in
  let xs = String.to_list s in
  assert (String.of_list xs = s);
  assert (String.fold List.push s [] = List.rev xs);
  assert (String.for_all Char.is_ascii s);
  assert (not (String.for_all Char.is_alpha s));
  assert (String.exists Char.is_punct s);
  assert (not (String.exists Char.is_digit s));
  assert (String.filter Char.is_alpha s = s');

  assert (String.skip_while Char.is_alpha s 8 = 12);
  assert (String.skip_until Char.is_space s 8 = 12);
  assert (String.rskip_while Char.is_alpha s 12 = 8);
  assert (String.rskip_until Char.is_space s 12 = 8);
  assert (String.skip_while Char.is_ascii s 0 = n);
  assert (String.rskip_while Char.is_ascii s n = 0);

  assert (String.slice 8 8 s = "");
  assert (String.slice 8 12 s = "make");
  assert (String.slice (n - 5) n s = "bugs.");
  assert (String.has_prefix "" s);
  assert (String.has_prefix "I won't" s);
  assert (not (String.has_prefix "n" s));
  assert (String.has_suffix "" s);
  assert (String.has_suffix "bugs." s);
  assert (not (String.has_suffix "?" s));
  assert (String.has_slice 8 "make" s);
  assert (String.find_slice "" s = Some 0);
  assert (String.find_slice "bugs" s = Some 13);
  assert (String.find_slice "bugS" s = None);
  ()
