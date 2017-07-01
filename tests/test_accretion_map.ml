(* Copyright (C) 2015--2017  Petter A. Urkedal <paurkedal@gmail.com>
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

module Int_order = struct type t = int let compare = compare end

module List_monoid = struct
  type 'a t = 'a list
  let empty = []
  let cat = (@)
end

module Am = Prime_accretion_map.Make1 (Int_order) (List_monoid)
module Rm = Map.Make (Int_order)

let test n =
  let aux i (rm, am) =
    let k = Random.int n in
    let x = [i] in
    Rm.add k x rm, Am.add k x am in
  let rm, am = Prime_int.fold_to aux n (Rm.empty, Am.empty) in
  assert (Am.cardinal am = Rm.cardinal rm);
  assert (Am.bindings am = Rm.bindings rm);
  assert (Am.result am = List.rev (Rm.fold (fun _ x acc -> x @ acc) rm []))

let run () =
  assert (Am.is_empty Am.empty);
  assert (not (Am.is_empty (Am.singleton 0 [])));
  assert (Am.cardinal Am.empty = 0);
  assert (Am.cardinal (Am.singleton 0 []) = 1);
  assert (Am.result Am.empty = []);
  assert (Am.result (Am.singleton 0 [11]) = [11]);
  for i = 1 to 200 do
    test i; test i; test i
  done
