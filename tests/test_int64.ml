(* Copyright (C) 2016--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

let rec alt_pow x n = if n = 0 then 1L else Int64.mul x (alt_pow x (n - 1))

let test_arith () =
  (* pow *)
  for n = 0 to 9 do
    assert (Prime_int64.pow 0L n = alt_pow 0L n);
    assert (Prime_int64.pow 1L n = 1L);
    assert (Prime_int64.pow 2L n = Int64.shift_left 1L n);
    assert (Prime_int64.pow (-3L) n = alt_pow (-3L) n);
    assert (Prime_int64.pow 5L n = alt_pow 5L n);
    assert (Prime_int64.pow (-7L) n = alt_pow (-7L) n)
  done

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
  test_arith ();
  test_bitcount ()
