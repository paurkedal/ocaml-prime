(* Copyright (C) 2013--2020  Petter A. Urkedal <paurkedal@gmail.com>
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
    mutable owner : Obj.t;
    mutable next : t;
    mutable access_count : int;
    mutable access_start : float;
    mutable grade : float;
  }

  let rec head = {
    owner = Obj.repr "head beacon";
    next = head;
    access_count = -1;
    access_start = -1.0;
    grade = 0.0;
  }

  let rec dummy = {
    owner = Obj.repr "dummy beacon";
    next = dummy;
    access_count = -2;
    access_start = -2.0;
    grade = 0.0;
  }

  let expire_all () = head.next <- head

  let overhead = Obj.size (Obj.repr head) + 3

  let check_beacon cs b =
    Prime_cache_metric.check cs b.access_count b.access_start b.grade

  let discard_depleted_beacons () =
    let cs = Prime_cache_metric.check_start M.cache_metric in
    let rec loop b =
      assert (b != dummy);
      if b.next == head then () else
      if check_beacon cs b.next then loop b.next else
      begin
        let b' = b.next in
        b.next <- b'.next;
        b'.next <- dummy;
        loop b
      end
    in
    loop head;
    Prime_cache_metric.check_stop cs

  let _gc_alarm = Gc.create_alarm discard_depleted_beacons

  let grade b = b.grade

  let set_grade g b =
    assert (b != dummy);
    b.grade <- g

  let charge b =
    if b.next == dummy then begin
      assert (b != dummy);
      b.next <- head.next;
      head.next <- b;
      b.access_start <-
        if b.access_count = 0 then
          Prime_cache_metric.access_init M.cache_metric
        else
          Prime_cache_metric.access_step M.cache_metric b.access_count
                                         b.access_start
    end else begin
      b.access_start <-
        Prime_cache_metric.access_step M.cache_metric b.access_count
                                       b.access_start
    end;
    b.access_count <- b.access_count + 1

  let embed g f =
    let b =
      { owner = Obj.repr head;
        next = dummy;
        access_count = 1;
        access_start = Prime_cache_metric.access_init M.cache_metric;
        grade = g; } in
    let obj = f b in
    b.owner <- Obj.repr obj;
    b.next <- head.next;
    head.next <- b;
    obj

  let cache_metric = M.cache_metric
end
