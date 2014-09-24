(* Copyright (C) 2013  Petter Urkedal <paurkedal@gmail.com>
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

(** Memory cache management for weak references ({e prime.testing}).

    Beacons can be used in conjunction with weak arrays or weak hash tables to
    implement memory caching schemes.  The beacon itself just prevent an
    object from being garbage-collected while it is actively accessed.  It
    does so by hooking beacons onto a global list which is filtered after each
    major GC. *)

module type S = sig

  type t
  (** The type of a field to embed in records in order to keep track of access
      and prevent actively used data from being garbage collected. *)

  val dummy : t
  (** A dummy beacon.  This is useful for temporary objects used as lookup keys
      for weak maps. *)

  val overhead : int
  (** The memory overhead of the beacon in machine words. *)

  val embed : float -> (t -> 'a) -> 'a
  (** [embed g f] passes a suitable beacon of grade [g] to [f], which is
      expected to construct and return an object embedding the beacon.
      Conversely, the returned object is made accessible from the beacon,
      which itself is kept visible to the garbage collector as long as [g] is
      greater than the memory pressure times the access period.  One access is
      recorded upon construction. *)

  val grade : t -> float
  (** [grade x] is the current grade of [x]. *)

  val set_grade : float -> t -> unit
  (** [set_grade g x] sets the grade of [x] to [g]. *)

  val charge : t -> unit
  (** [charge b] records the fact that [b] has just been accessed.  This is
      typically called each time an object is acquired from a weak data
      structure. *)

end

module type CACHE_METRIC = sig val cache_metric : Prime_cache_metric.t end

module Make (M : CACHE_METRIC) : S
(** Create a beacon API for based on the given metrics.  The resulting module
    contains globals and registers a [Gc.alarm], so it is recommended to use a
    single or a few global instances. *)
