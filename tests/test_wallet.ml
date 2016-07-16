(* Copyright (C) 2014--2016  Petter A. Urkedal <paurkedal@gmail.com>
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

open Prime_wallet

let f i = 2*i + 1
let g i = 3 - i

let test n =
  (* Test empty and singular. *)
  assert (is_empty empty);
  assert (pop (singleton 2) = (2, empty));
  assert (push 2 empty = singleton 2);
  assert (length empty = 0);
  assert (length (singleton 2) = 1);

  (* Test push and pop. *)
  let rec build i wf =
    if i < 0 then wf else begin
      let wf' = push (f i) wf in
      assert (pop wf' = (f i, wf));
      build (i - 1) wf'
    end in
  let wf = build (n - 1) empty in
  assert (length wf = n);

  (* Test get. *)
  for i = 0 to n - 1 do
    assert (get wf i = f i)
  done;

  (* Test sample and map. *)
  let wg = sample g n in
  assert (wf = map (fun j -> 2*(3 - j) + 1) wg);
  let wg', wf' =
    let rec loop i wg wf =
      if i = n then (wg, wf) else
      if Random.bool () then loop (i + 1) (set i (f i) wg) wf
                        else loop (i + 1) wg (set i (g i) wf) in
    loop 0 wg wf in
  assert (wg' = wf');

  (* Test iterators. *)
  let ir = ref 0 in
  iter (fun j -> assert (j = f !ir); ir := !ir + 1) wf;
  assert (!ir = n);
  let n' = fold (fun j i -> assert (j = f i); (i + 1)) wf 0 in
  assert (n' = n);
  let wg'' = mapj (fun i j -> assert (j = f i); g i) 0 wf in
  assert (wg'' = wg);

  (* Test copush and copop. *)
  let nC = split_snd_length n in
  let rec cobuild i cowf =
    if i = n then cowf else begin
      let j = i + (n - nC) in
      let cowf' = copush (f j) cowf in
      assert (copop cowf' = (f j, cowf));
      cobuild (i + 1) cowf'
    end in
  let cowf = cobuild 0 empty in
  assert (length cowf = n);

  (* Test split and counterpart consistency. *)
  let wfR, wfC = split wf in
  assert (length wfC = nC);
  assert (length wfR = n - nC);
  let cowfR, cowfC = cosplit cowf in
  assert (length cowfC = nC);
  assert (length cowfR = n - nC);
  assert (wfC = cowfC)

let run () =
  for n = 1 to 65 do test n done;
  List.iter test [128; 175; 921; 5000]
