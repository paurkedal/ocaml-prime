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

let random_bits64 () =
  let a = Random.int64 (Int64.shift_left 1L 32) in
  let b = Random.int64 (Int64.shift_left 1L 32) in
  Int64.(add a (shift_left b 32))

let test_bitcount () =
  assert (Prime_int64.bitcount (Int64.lognot 0L) = 64);
  let rec count_bits x acc =
    if x = 0L then acc else
    count_bits Int64.(shift_right_logical x 1)
               (acc + Int64.(to_int (logand x 1L))) in
  for _ = 0 to 9999 do
    let x = random_bits64 () in
    assert (Prime_int64.bitcount x = count_bits x 0)
  done

let run () =
  test_bitcount ()
