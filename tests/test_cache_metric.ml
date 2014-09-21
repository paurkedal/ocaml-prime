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

open Printf

module Cm = Prime_cache_metric

let current_time_r = ref 0.0
let current_time () = !current_time_r

let p = 100.0
let current_memory_pressure () = p

let (=~) x y =
  abs_float (x -. y) /. (abs_float x +. abs_float y) < 1e-6

let run () =
  let cm = Cm.create ~current_time ~current_memory_pressure () in
  let access_start = ref (Cm.access_init cm) in
  for i = 1 to 32 do
    (* Check *)
    current_time_r := float_of_int i -. 0.5;
    let cmt = Cm.check_prep cm in
    if false then
      printf "%6.3g %6.3g %6.3g\n" !current_time_r !access_start
				   (Cm.check cmt i !access_start)
    else
      assert (Cm.check cmt i !access_start =~ p);
    (* Step *)
    current_time_r := float_of_int i;
    access_start := Cm.access_step cm i !access_start
  done
