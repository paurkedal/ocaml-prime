(* Copyright (C) 2013--2019  Petter A. Urkedal <paurkedal@gmail.com>
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
include Unprime_list

let run () =
  assert_equal
    [1; 2; 5]
    (List.filter_map (fun x -> if x >= 0 then Some x else None) [1; 2; -3; -4; 5]);
  assert_equal
    [3; 2; 1; 0; 1; 0; 2; 1; 0]
    (List.flatten_map (fun n -> Prime_int.fold_to List.cons n []) [4; 2; 3]);
  assert_equal
    [-1; 0; 1; 2; 3; 4; 5]
    (List.rev_flatten [[5; 4; 3]; [2; 1]; [0; -1]]);

  (* concat *)
  assert (List.interfix 0 [1] = [1]);
  assert (List.interfix 0 [1; 2] = [1; 0; 2]);
  assert (List.interfix 0 [1; 2; 3] = [1; 0; 2; 0; 3])
