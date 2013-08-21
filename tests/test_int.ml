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

include OUnit

let assert_equal_int = assert_equal ~printer:string_of_int

let run () =
  (* Assumed by the implementation of fmod: *)
  assert_equal_int ((min_int-1) lxor min_int) (-1);

  (* fdiv *)
  assert_equal_int ~msg:"5 / 2"   2 (Prime_int.fdiv 5 2);
  assert_equal_int ~msg:"-5 / -2" 2 (Prime_int.fdiv (-5) (-2));
  assert_equal_int ~msg:"-5 / 2" (-3) (Prime_int.fdiv (-5) 2);
  assert_equal_int ~msg:"5 / -2" (-3) (Prime_int.fdiv 5 (-2));

  (* fmod *)
  assert_equal_int ~msg:"5 mod 2"   1 (Prime_int.fmod 5 2);
  assert_equal_int ~msg:"-5 mod -2" (-1) (Prime_int.fmod (-5) (-2));
  assert_equal_int ~msg:"-5 mod 2"  1 (Prime_int.fmod (-5) 2);
  assert_equal_int ~msg:"5 mod -2"  (-1) (Prime_int.fmod 5 (-2));

  (* fdiv and fmod randomised *)
  for round = 0 to 9999 do
    let x = Random.bits () in
    let y = Random.bits () in
    if y > 0 then begin
      let q, r = Prime_int.fdiv x y, Prime_int.fmod x y in
      assert_equal_int ~msg:"y * q + r = x" x (y * q + r);
      assert_equal ~msg:"r has the same sign as y" (y < 0) (r < 0)
    end
  done
