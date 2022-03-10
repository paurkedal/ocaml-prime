(* Copyright (C) 2019--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

let run () =
  for i = 0 to 255 do
    let c = Char.chr i in
    let open Prime_char in
    assert (is_ascii c = (i < 128));
    assert (not (is_ascii_graph c && is_ascii_cntrl c));
    assert (not (is_ascii_graph c && is_ascii_space c));
    assert (not (is_ascii_alpha c && is_ascii_digit c));
    assert (not (is_ascii_lower c && is_ascii_upper c));
    assert (not (is_ascii_alpha c && is_ascii_punct c));
    assert (not (is_ascii_digit c && is_ascii_punct c));
    assert (not (is_ascii_space c && is_ascii_punct c));
    assert (is_ascii c = (is_ascii_cntrl c || is_ascii_space c
                                           || is_ascii_graph c));
    assert (is_ascii_graph c = (is_ascii_alnum c || is_ascii_punct c));
    assert (is_ascii_alnum c = (is_ascii_alpha c || is_ascii_digit c));
    assert (is_ascii_alpha c = (is_ascii_lower c || is_ascii_upper c));
    assert (is_xdigit c = (is_ascii_digit c || 'a' <= c && c <= 'f'
                                            || 'A' <= c && c <= 'F'))
  done
