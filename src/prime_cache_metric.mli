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

type t

type check_state = private {
  cs_cm : t;
  cs_time : float;
  cs_memory_pressure : float;
  mutable cs_live_count : int;
  mutable cs_dead_count : int;
}

val create : ?period_sample_size: int ->
	     current_time: (unit -> float) ->
	     current_memory_pressure: (unit -> float) ->
	     ?report: (check_state -> unit) ->
	     unit -> t

val access_init : t -> float
val access_step : t -> int -> float -> float

val check_start : t -> check_state
val check : check_state -> int -> float -> float -> bool
val check_stop : check_state -> unit
