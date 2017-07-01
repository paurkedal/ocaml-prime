(* Copyright (C) 2016--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

let random_bits32 () =
  let a = Random.int32 (Int32.shift_left 1l 16) in
  let b = Random.int32 (Int32.shift_left 1l 16) in
  Int32.(add a (shift_left b 16))

let test_bitcount () =
  assert (Prime_int32.bitcount (Int32.lognot 0l) = 32);
  let rec count_bits x acc =
    if x = 0l then acc else
    count_bits Int32.(shift_right_logical x 1)
               (acc + Int32.(to_int (logand x 1l))) in
  for _ = 0 to 9999 do
    let x = random_bits32 () in
    assert (Prime_int32.bitcount x = count_bits x 0)
  done

let run () =
  test_bitcount ()
