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

let rec alt_pow x n = if n = 0 then 1l else Int32.mul x (alt_pow x (n - 1))

let test_arith () =
  (* pow *)
  for n = 0 to 9 do
    assert (Prime_int32.pow 0l n = alt_pow 0l n);
    assert (Prime_int32.pow 1l n = 1l);
    assert (Prime_int32.pow 2l n = Int32.shift_left 1l n);
    assert (Prime_int32.pow (-3l) n = alt_pow (-3l) n);
    assert (Prime_int32.pow 5l n = alt_pow 5l n);
    assert (Prime_int32.pow (-7l) n = alt_pow (-7l) n)
  done

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
  test_arith ();
  test_bitcount ()
