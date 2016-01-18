(* Copyright (C) 2016  Petter A. Urkedal <paurkedal@gmail.com>
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

let test_fraction () =
  assert (Prime_float.to_fraction 0.0 = (0, 1));
  let aux i j =
    let x = float_of_int i /. float_of_int j in
    let n, d = Prime_float.to_fraction x in
    let g = Prime_int.gcd i j in
    assert (n = i / g);
    assert (d = j / g) in
  for i = 1 to 199 do
    for j = 1 to 199 do
      aux i j
    done
  done;
  for i = 1 to 10000 do
    aux (Random.int 10000 + 200) (Random.int 10000 + 200)
  done

let run () =
  test_fraction ()
