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

module Int_order = struct
  type t = int
  let compare = compare
end

module Q = Prime_priqueue.Make (Int_order)

module R = struct
  module M = Map.Make (Int_order)
  let empty = M.empty
  let is_empty = M.is_empty
  let add x m = M.add x (try M.find x m + 1 with Not_found -> 1) m
  let remove x m =
    try
      let n = M.find x m in
      if n = 1 then M.remove x m else M.add x (n - 1) m
    with Not_found -> m
  let find_min m = fst (M.min_binding m)
  let remove_min m = remove (find_min m) m
end

let rec test n i q r =
  if i < n then begin
    let q', r' =
      match Random.int (if Q.is_empty q then 2 else 3) with
      | 0 -> let x = Random.int n in (Q.add x q, R.add x r)
      | 1 -> let x = Random.int n in (Q.remove x q, R.remove x r)
      | 2 -> (Q.remove_min q, R.remove_min r)
      | _ -> assert false in
    assert (Q.is_empty q' = R.is_empty r');
    if not (Q.is_empty q') then assert (Q.find_min q' = R.find_min r');
    test n (i + 1) q' r'
  end

let run () =
  for _ = 0 to 999 do
    test (Random.int 200) 0 Q.empty R.empty
  done
