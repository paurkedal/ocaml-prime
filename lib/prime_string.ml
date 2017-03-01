(* Copyright (C) 2013--2017  Petter A. Urkedal <paurkedal@gmail.com>
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
open Unprime

let sample f n = String.init n f

let of_chars xs =
  let n = List.length xs in
  let xs_r = ref xs in
  String.init n @@ fun i ->
    match !xs_r with
    | [] -> assert false
    | x :: xs -> xs_r := xs; x

let to_chars s =
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

let foldi f s accu =
  let accu_r = ref accu in
  for i = 0 to length s - 1 do
    accu_r := f i s.[i] !accu_r
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

let search f s =
  let n = length s in
  let rec search_from i =
    if i = n then None else
    match f s.[i] with
    | None -> search_from (i + 1)
    | y -> y in
  search_from 0

let rec skip_while f s i =
  if i < length s && f s.[i] then skip_while f s (i + 1) else i

let rec skip_until f s i =
  if i < length s && not (f s.[i]) then skip_until f s (i + 1) else i

let rec rskip_while f s i =
  if i > 0 && f s.[i - 1] then rskip_while f s (i - 1) else i

let rec rskip_until f s i =
  if i > 0 && not (f s.[i - 1]) then rskip_until f s (i - 1) else i

let slice i j s = sub s i (j - i)

let slice_from i s = sub s i (String.length s - i)

let has_slice j sce s =
  let n = length sce in
  if length s - j < n then false else
  let rec loop i = i >= n || s.[j + i] = sce.[i] && loop (i + 1) in
  loop 0

let skip_affix afx s i =
  let m, n = String.length afx, String.length s in
  let rec loop i l =
    if l = m then Some (i + m) else
    if s.[i + l] = afx.[l] then loop i (l + 1) else
    if i < n - m then loop (i + 1) 0 else
    None in
  if i < 0 then invalid_arg "Prime_string.skip_affix: Negative index." else
  if i <= n - m then loop i 0 else
  if i > n then invalid_arg "Prime_string.skip_affix: Index past EOS." else
  None

let rskip_affix afx s j =
  let m, n = String.length afx, String.length s in
  let rec loop i l =
    if l = m then Some i else
    if s.[i + l] = afx.[l] then loop i (l + 1) else
    if i > 0 then loop (i - 1) 0 else
    None in
  if j < 0 then invalid_arg "Prime_string.rskip_affix: Negative index." else
  if j >= m then loop (j - m) 0 else
  if j > n then invalid_arg "Prime_string.rskip_affix: Index past EOS." else
  None

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

let cut_affix afx s =
  let m, n = length afx, length s in
  skip_affix afx s 0 |?>. fun j ->
  (slice 0 (j - m) s, slice j n s)

let rcut_affix afx s =
  let m, n = length afx, length s in
  rskip_affix afx s n |?>. fun i ->
  (slice 0 i s, slice (i + m) n s)

let chop_affix afx s =
  let m = length afx in
  let rec loop i j acc =
    if i < 0 then slice 0 j s :: acc else
    if has_slice i afx s then loop (i - m) i (slice (i + m) j s :: acc) else
    loop (i - 1) j acc in
  let n = length s in
  if n = 0 then [] else loop (n - m) n []

let cut_consecutive f s =
  let n = length s in
  let i = skip_until f s 0 in if i = n then None else
  let j = skip_while f s i in if i = 0 && j = n then None else
  Some (slice 0 i s, slice j n s)

let rcut_consecutive f s =
  let n = length s in
  let j = rskip_until f s n in if j = 0 then None else
  let i = rskip_while f s j in if j = n && i = 0 then None else
  Some (slice 0 i s, slice j n s)

let chop_consecutive f s =
  let rec loop j acc =
    if j = 0 then acc else
    let i = rskip_until f s j in
    loop (rskip_while f s i) (slice i j s :: acc) in
  loop (rskip_while f s (length s)) []

(**/**)
let of_list = of_chars
let to_list = to_chars
