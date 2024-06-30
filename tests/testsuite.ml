(* Copyright (C) 2013--2024  Petter A. Urkedal <paurkedal@gmail.com>
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

let tests = [
  "test_array", Test_array.test_cases;
  "test_accretion_map", Test_accretion_map.test_cases;
  "test_beacon", Test_beacon.test_cases;
  "test_cache_metric", Test_cache_metric.test_cases;
  "test_char", Test_char.test_cases;
  "test_enumlist", Test_enumlist.test_cases;
  "test_enummap", Test_enummap.test_cases;
  "test_enumset", Test_enumset.test_cases;
  "test_float", Test_float.test_cases;
  "test_int", Test_int.test_cases;
  "test_int32", Test_int32.test_cases;
  "test_int64", Test_int64.test_cases;
  "test_list", Test_list.test_cases;
  "test_map", Test_map.test_cases;
  "test_priqueue", Test_priqueue.test_cases;
  "test_retraction", Test_retraction.test_cases;
  "test_string", Test_string.test_cases;
  "test_wallet", Test_wallet.test_cases;
]

let () = Random.self_init (); Alcotest.V1.run "prime" tests
