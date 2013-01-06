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

open String

let sample f n =
  let s = String.create n in
  for i = 0 to n - 1 do s.[i] <- f i done;
  s

let of_list xs =
  let n = List.length xs in
  let s = String.create n in
  let xs_r = ref xs in
  for i = 0 to n - 1 do
    s.[i] <- List.hd !xs_r;
    xs_r := List.tl !xs_r
  done;
  s

let to_list s =
  let zs_r = ref [] in
  for i = String.length s - 1 downto 0 do
    zs_r := s.[i] :: !zs_r
  done;
  !zs_r

let fold f s accu =
  let accu_r = ref accu in
  for i = 0 to length s - 1 do
    accu_r := f s.[i] !accu_r
  done;
  !accu_r

let for_all f s =
  let n = length s in
  let rec loop i = i >= n || f s.[i] && loop (i + 1) in
  loop 0

let exists f s =
  let n = length s in
  let rec loop i = i < n && (f s.[i] || loop (i + 1)) in
  loop 0

let filter f s =
  let buf = Buffer.create (length s) in
  for i = 0 to length s - 1 do
    let ch = s.[i] in
    if f ch then Buffer.add_char buf ch
  done;
  Buffer.contents buf

let rec skip_while f s i =
  if i < length s && f s.[i] then skip_while f s (i + 1) else i

let rec skip_until f s i =
  if i < length s && not (f s.[i]) then skip_until f s (i + 1) else i

let rec rskip_while f s i =
  if i > 0 && f s.[i - 1] then rskip_while f s (i - 1) else i

let rec rskip_until f s i =
  if i > 0 && not (f s.[i - 1]) then rskip_until f s (i - 1) else i

let slice i j s = sub s i (j - i)

let has_slice j sce s =
  let n = length sce in
  if length s - j < n then false else
  let rec loop i = i >= n || s.[j + i] = sce.[i] && loop (i + 1) in
  loop 0

let has_prefix pfx s =
  let n = length pfx in
  if length s < n then false else
  let rec loop i = i >= n || s.[i] = pfx.[i] && loop (i + 1) in
  loop 0

let has_suffix sfx s =
  let n = length sfx in
  let j = length s - n in
  if j < 0 then false else
  let rec loop i = i >= n || s.[j + i] = sfx.[i] && loop (i + 1) in
  loop 0

let find_slice ?(start = 0) sce s =
  let n = length sce in
  let m = length s - n in
  let rec loop i =
    if i > m then None else
    if has_slice i sce s then Some i else
    loop (i + 1) in
  loop start
