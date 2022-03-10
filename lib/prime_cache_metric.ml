(* Copyright (C) 2014--2022  Petter A. Urkedal <paurkedal@gmail.com>
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

type t = {
  cm_period_sample_size : int;
  cm_w_past : float;
  cm_w_step : float;
  cm_renorm : float;
  cm_current_time : unit -> float;
  cm_current_memory_pressure : unit -> float;
  cm_report : check_state -> unit;
}
and check_state = {
  cs_cm : t;
  cs_time : float;
  cs_memory_pressure : float;
  mutable cs_live_count : int;
  mutable cs_dead_count : int;
}

let create ?(period_sample_size = 16)
           ~current_time ~current_memory_pressure ?(report = fun _ -> ()) () =
  let n = float_of_int period_sample_size in
  { cm_period_sample_size = period_sample_size;
    cm_w_past = (n -. 1.0) /. n;
    cm_w_step = 1.0 /. n;
    cm_renorm = 1.0 /. (n -. 0.5);
    cm_current_time = current_time;
    cm_current_memory_pressure = current_memory_pressure;
    cm_report = report; }

let access_init cm = cm.cm_current_time ()

let access_step cm access_count access_time =
  if access_count >= cm.cm_period_sample_size then
       cm.cm_w_past *. access_time
    +. cm.cm_w_step *. cm.cm_current_time ()
  else
    access_time

let check_start cm =
  { cs_cm = cm;
    cs_time = cm.cm_current_time ();
    cs_memory_pressure = cm.cm_current_memory_pressure ();
    cs_live_count = 0;
    cs_dead_count = 0; }

let check cs access_count access_time g =
  let dt =
    let dt_hist = cs.cs_time -. access_time in
    if access_count >= cs.cs_cm.cm_period_sample_size
    then dt_hist *. cs.cs_cm.cm_renorm
    else dt_hist /. (float_of_int access_count -. 0.5) in
  if dt *. cs.cs_memory_pressure < g
  then (cs.cs_live_count <- cs.cs_live_count + 1; true)
  else (cs.cs_dead_count <- cs.cs_dead_count + 1; false)

let check_stop cs = cs.cs_cm.cm_report cs
