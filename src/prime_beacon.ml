(* Copyright (C) 2013--2014  Petter Urkedal <paurkedal@gmail.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *)

open Printf

module type CACHE_METRIC = sig
  val cache_metric : Prime_cache_metric.t
end

module type S = sig
  type t
  val expire_all : unit -> unit
  val dummy : t
  val overhead : int
  val embed : float -> (t -> 'a) -> 'a
  val grade : t -> float
  val set_grade : float -> t -> unit
  val charge : t -> unit
  val cache_metric : Prime_cache_metric.t
end

module Make (M : CACHE_METRIC) = struct

  type t = {
    mutable b_owner : Obj.t;
    mutable b_next : t;
    mutable b_access_count : int;
    mutable b_access_start : float;
    mutable b_grade : float;
  }

  let rec head = {
    b_owner = Obj.repr "head beacon";
    b_next = head;
    b_access_count = -1;
    b_access_start = -1.0;
    b_grade = 0.0;
  }

  let rec dummy = {
    b_owner = Obj.repr "dummy beacon";
    b_next = dummy;
    b_access_count = -2;
    b_access_start = -2.0;
    b_grade = 0.0;
  }

  let expire_all () = head.b_next <- head

  let overhead = Obj.size (Obj.repr head) + 3

  let check_beacon cs b =
    Prime_cache_metric.check cs b.b_access_count b.b_access_start b.b_grade

  let discard_depleted_beacons () =
    let cs = Prime_cache_metric.check_start M.cache_metric in
    let rec loop b =
      if b.b_next == head then () else
      if check_beacon cs b.b_next then loop b.b_next else
      begin
	let b' = b.b_next in
	b.b_next <- b.b_next.b_next;
	b'.b_next <- dummy;
	loop b
      end in
    loop head;
    Prime_cache_metric.check_stop cs

  let gc_alarm = Gc.create_alarm discard_depleted_beacons

  let grade {b_grade} = b_grade

  let set_grade g b =
    assert (b != dummy);
    b.b_grade <- g

  let charge b =
    if b.b_next == dummy then begin
      assert (b != dummy);
      b.b_next <- head.b_next;
      head.b_next <- b;
      b.b_access_start <-
	if b.b_access_count = 0 then
	  Prime_cache_metric.access_init M.cache_metric
	else
	  Prime_cache_metric.access_step M.cache_metric b.b_access_count
					 b.b_access_start
    end else begin
      b.b_access_start <-
	Prime_cache_metric.access_step M.cache_metric b.b_access_count
				       b.b_access_start
    end;
    b.b_access_count <- b.b_access_count + 1

  let embed g f =
    let b =
      { b_owner = Obj.repr head;
	b_next = head.b_next;
	b_access_count = 1;
	b_access_start = Prime_cache_metric.access_init M.cache_metric;
	b_grade = g; } in
    let obj = f b in
    b.b_owner <- Obj.repr obj;
    head.b_next <- b;
    obj

  let cache_metric = M.cache_metric
end
