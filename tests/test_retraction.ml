(* Copyright (C) 2014  Petter Urkedal <paurkedal@gmail.com>
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

module R = Prime_retraction.Make
  (struct
    type t = int
    type key = int
    let compare = compare
    let compare_key = compare
  end)

let dump label r =
  Printf.printf "%s =" label;
  R.iter (fun e -> Printf.printf " %d" e) r;
  print_newline ()

let random_retraction n_max =
  let n = Random.int n_max in
  Prime_int.fold_to (fun _ -> R.add (Random.int n_max)) n R.empty

let test_alg () =
  let n_max = 1 lsl Random.int 8 in
  let rA = random_retraction n_max in
  let rB = random_retraction n_max in
  let rAuB = R.funion (fun eA eB -> Some eA) rA rB in
  let rAnB = R.finter (fun eA eB -> Some eA) rA rB in
  let rAsB = R.funion (fun eA eB -> None) rA rB in (* sym. diff *)
  let rAcB = R.fcompl (fun eA eB -> None) rA rB in
  let rAuB' = R.fold R.add rB rA in
  let rAnB' = R.filter (fun e -> R.contains e rA) rB in
  let rAcB' = R.fold R.remove rA rB in
  assert (R.equal rAuB rAuB');
  assert (R.equal rAnB rAnB');
  assert (R.equal rAcB rAcB');
  assert (R.cardinal rAuB = R.cardinal rAnB + R.cardinal rAsB);
  assert (R.cardinal rAcB = R.cardinal rB - R.cardinal rAnB)

let run () =
  assert (R.equal R.empty R.empty);
  assert (R.compare R.empty (R.singleton 0) = -1);
  assert (R.compare (R.singleton 0) R.empty = 1);
  assert (R.compare (R.singleton 0) (R.singleton 1) = -1);
  assert (R.compare (R.singleton 1) (R.singleton 0) = 1);
  for round = 0 to 9999 do
    test_alg ()
  done
