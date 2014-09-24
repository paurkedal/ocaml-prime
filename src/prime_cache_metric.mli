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

(** Parameters and measuring functions for memory caches
    ({e prime.testing}). *)

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
(** Creates specifications for how to measure whether objects are worth
    caching.  The returned object can be used by {!Prime_cache} and
    {!Prime_beacon}.

    @param period_sample_size The approximate number of accesses averaged over
    in the estimate of the expected period before the next access.

    @param current_time The current time according to a clock which is
    considered relevant for computing the cost of using memory for caching.

    @param current_memory_pressure The current cost of using one unit of
    memory as cache for one unit of time.

    @param report This function is called after a collection with information
    about how it went. *)

(** {2 Internal Functions}

    The remaining functions of this module are {e subject to change at any
    time}.  They and are not needed for using the predefined caches. *)

val access_init : t -> float
val access_step : t -> int -> float -> float

val check_start : t -> check_state
val check : check_state -> int -> float -> float -> bool
val check_stop : check_state -> unit
