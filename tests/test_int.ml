(* Copyright (C) 2013--2018  Petter A. Urkedal <paurkedal@gmail.com>
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

open OUnit
open Utils

let verbose = false

let rec alt_pow x n = if n = 0 then 1 else x * alt_pow x (n - 1)

let test_arith () =
  (* pow *)
  for n = 0 to 9 do
    assert (Prime_int.pow 0 n = Prime_int.delta 0 n);
    assert (Prime_int.pow 1 n = 1);
    assert (Prime_int.pow 2 n = 1 lsl n);
    assert (Prime_int.pow (-3) n = alt_pow (-3) n);
    assert (Prime_int.pow 5 n = alt_pow 5 n);
    assert (Prime_int.pow (-7) n = alt_pow (-7) n)
  done;

  (* Assumed by the implementation of fmod: *)
  assert_equal_int ((min_int-1) lxor min_int) (-1);

  (* fdiv *)
  assert_equal_int ~msg:"⌊7 / 3⌋"   2 (Prime_int.fdiv 7 3);
  assert_equal_int ~msg:"⌊-7 / -3⌋" 2 (Prime_int.fdiv (-7) (-3));
  assert_equal_int ~msg:"⌊-7 / 3⌋" (-3) (Prime_int.fdiv (-7) 3);
  assert_equal_int ~msg:"⌊7 / -3⌋" (-3) (Prime_int.fdiv 7 (-3));

  (* fmod *)
  assert_equal_int ~msg:"7 fmod 3"   1 (Prime_int.fmod 7 3);
  assert_equal_int ~msg:"-7 fmod -3" (-1) (Prime_int.fmod (-7) (-3));
  assert_equal_int ~msg:"-7 fmod 3"  2 (Prime_int.fmod (-7) 3);
  assert_equal_int ~msg:"7 fmod -3"  (-2) (Prime_int.fmod 7 (-3));

  (* cdiv *)
  assert_equal_int ~msg:"⌈7 / 3⌉"   3 (Prime_int.cdiv 7 3);
  assert_equal_int ~msg:"⌈-7 / -3⌉" 3 (Prime_int.cdiv (-7) (-3));
  assert_equal_int ~msg:"⌈-7 / 3⌉" (-2) (Prime_int.cdiv (-7) 3);
  assert_equal_int ~msg:"⌈7 / -3⌉" (-2) (Prime_int.cdiv 7 (-3));

  (* cmod *)
  assert_equal_int ~msg:"7 cmod 3"  (-2) (Prime_int.cmod 7 3);
  assert_equal_int ~msg:"-7 cmod -3"  2 (Prime_int.cmod (-7) (-3));
  assert_equal_int ~msg:"-7 cmod 3" (-1) (Prime_int.cmod (-7) 3);
  assert_equal_int ~msg:"7 cmod -3"   1 (Prime_int.cmod 7 (-3));

  (* fdiv and fmod randomised *)
  for _ = 0 to 9999 do
    let ex, ey = Random.int 29, Random.int 29 in
    let x = Random.int (1 lsl ex) - (1 lsl (ex - 1)) in
    let y = Random.int (1 lsl ey) - (1 lsl (ey - 1)) in
    if y > 0 then begin
      let q, r = Prime_int.fdiv x y, Prime_int.fmod x y in
      assert_equal_int ~msg:"y * ⌊x / y⌋ + x fmod y = x" x (y * q + r);
      assert_equal ~msg:"(x fmod y) has the same sign as y" (y < 0) (r < 0);
      let q, r = Prime_int.cdiv x y, Prime_int.cmod x y in
      assert_equal_int ~msg:"y * ⌈x / y⌉ + x cmod y = x" x (y * q + r);
      assert_equal ~msg:"(x cmod y) has the opposite sign of y" (y < 0) (r > 0)
    end
  done

let test_gcd () =
  assert (Prime_int.gcd 0 0 = 0);
  for j = 1 to 500 do
    assert (Prime_int.gcd 0 j = j);
    assert (Prime_int.gcd j 0 = j);
    assert (Prime_int.gcd 0 (- j) = j);
    assert (Prime_int.gcd (- j) 0 = j);
    for i = 0 to j do
      let k = Prime_int.gcd i j in
      assert (k = Prime_int.gcd j i);
      assert (k = Prime_int.gcd i (- j));
      assert (k = Prime_int.gcd (- i) j);
      assert (k = Prime_int.gcd (- i) (- j));
      assert (i mod k = 0);
      assert (j mod k = 0);
      let i', j' = i / k, j / k in
      assert (Prime_int.gcd i' j' = 1);
      assert (i' mod 2 > 0 || j' mod 2 > 0);
      assert (i' mod 3 > 0 || j' mod 3 > 0);
      assert (i' mod 5 > 0 || j' mod 5 > 0);
      assert (i' mod 7 > 0 || j' mod 7 > 0);
      assert (i' mod 11 > 0 || j' mod 11 > 0);
      assert (i' mod 13 > 0 || j' mod 13 > 0)
    done
  done

let test_signed_width () =
  let x = (1 lsl (Prime_int.signed_width - 1) - 1) lsl 1 + 1 in
  assert (x > 0);
  assert (-x < 0);
  assert (Prime_int.bitcount x = Prime_int.signed_width);
  assert (x lsl 1 <= x)

let test_bitcount () =
  let rec count_bits x acc =
    if x = 0 then acc else
    count_bits (x lsr 1) (acc + x land 1) in
  for _ = 0 to 9999 do
    let x = Random.bits () in
    assert (Prime_int.bitcount x = count_bits x 0)
  done

let test_floor_ceil_log2 () =

  (* floor_log2 and ceil_log2 *)
  for n = 1 to 10000 do
    let i, j = Prime_int.floor_log2 n, Prime_int.ceil_log2 n in
    if i = j then assert_equal_int n (1 lsl i) else
    begin
      assert_equal_int 1 (j - i);
      assert (1 lsl i < n);
      assert (n < 1 lsl j)
    end
  done

let test_binom () =
  let n_max = 70 in
  let pascal = Array.init (n_max + 1) (fun _ -> 0) in
  pascal.(0) <- 1;
  if verbose then Printf.printf "\n";
  for n = 0 to n_max do
    if verbose then Printf.printf "%*s" (3 * (n_max - n)) "";
    for k = n downto 1 do
      begin
        try
          let b = Prime_int.binom n k in
          pascal.(k) <- pascal.(k - 1) + pascal.(k);
          assert (b = pascal.(k))
        with Failure _ ->
          if pascal.(k) <> -1 then begin
            assert (pascal.(k) + pascal.(k - 1) < pascal.(k));
            pascal.(k) <- -1
          end
      end;
      if verbose then Printf.printf "%6d" pascal.(k)
    done;
    if verbose then Printf.printf "%6d\n%!" pascal.(0)
  done

let run () =
  test_arith ();
  test_gcd ();
  test_signed_width ();
  test_bitcount ();
  test_floor_ceil_log2 ();
  test_binom ()
