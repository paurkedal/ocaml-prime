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

type t = {
  cm_period_sample_size : int;
  cm_w_past : float;
  cm_w_step : float;
  cm_renorm : float;
  cm_current_time : unit -> float;
  cm_current_memory_pressure : unit -> float;
}

type u = t * float * float

let create ?(period_sample_size = 16)
	   ~current_time ~current_memory_pressure () =
  let n = float_of_int period_sample_size in
  { cm_period_sample_size = period_sample_size;
    cm_w_past = (n -. 1.0) /. n;
    cm_w_step = 1.0 /. n;
    cm_renorm = 1.0 /. (n -. 0.5);
    cm_current_time = current_time;
    cm_current_memory_pressure = current_memory_pressure; }

let access_init cm = cm.cm_current_time ()

let access_step cm access_count access_time =
  if access_count >= cm.cm_period_sample_size then
       cm.cm_w_past *. access_time
    +. cm.cm_w_step *. cm.cm_current_time ()
  else
    access_time

let check_prep cm =
  (cm, cm.cm_current_time (), cm.cm_current_memory_pressure ())

let check (cm, t, p) access_count access_time =
  let dt =
    let dt_hist = t -. access_time in
    if access_count >= cm.cm_period_sample_size
    then dt_hist *. cm.cm_renorm
    else dt_hist /. (float_of_int access_count -. 0.5) in
  dt *. p
